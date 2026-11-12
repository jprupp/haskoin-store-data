{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a review of the package found in November 2026, each held here
-- by the input that showed it: values that failed with 'error' after
-- their parser had answered, numbers that wrapped around, a loop, and a
-- client that lost what the server said.
module Haskoin.Store.RobustSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_, void)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as A
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.Bytes.Get (runGetS)
import Data.Bytes.Serial (deserialize)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntMap qualified as IntMap
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Haskoin
import Haskoin.Store.Data
import Haskoin.Store.Gen
import Haskoin.Store.WebClient
import Haskoin.Store.WebCommon
import Haskoin.Util.Arbitrary
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S
import Test.Hspec
import Test.QuickCheck (generate)

-- | Change a field of an object.
with :: A.Key -> A.Value -> A.Value -> A.Value
with k v (A.Object o) = A.Object (KeyMap.insert k v o)
with _ _ other = other

failed :: Either String a -> Bool
failed = either (const True) (const False)

-- | A server that answers one request with these pieces, a moment apart,
-- and closes. The action is given its address.
serving :: [B.ByteString] -> (String -> IO a) -> IO a
serving pieces go =
  bracket open S.close $ \sock -> do
    port <- S.socketPort sock
    void . forkIO $ do
      (conn, _) <- S.accept sock
      void (S.recv conn 65536)
      forM_ pieces $ \piece -> S.sendAll conn piece >> threadDelay 50000
      S.close conn
    go ("http://127.0.0.1:" <> show port <> "/")
  where
    open = do
      sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
      S.setSocketOption sock S.ReuseAddr 1
      S.bind sock (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, 1)))
      S.listen sock 1
      return sock

-- | The head of a response whose body comes in chunks.
chunked :: B.ByteString -> B.ByteString
chunked status =
  "HTTP/1.1 " <> status <> "\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\n\r\n"

chunk :: B.ByteString -> B.ByteString
chunk bytes = C.pack (hex (B.length bytes)) <> "\r\n" <> bytes <> "\r\n"
  where
    hex n = let (q, r) = n `divMod` 16 in (if q > 0 then hex q else "") <> [("0123456789abcdef" !! r)]

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "Encoders" $ do
    it "write a peer whose user agent is not UTF-8" $ do
      let peer =
            PeerInfo
              { userAgent = B.pack [0x2f, 0xff, 0xfe, 0x2f],
                address = "10.0.0.1:8333",
                version = 70015,
                services = 1,
                relay = True
              }
      BL.length (A.encode peer) `shouldSatisfy` (> 0)
      BL.length (A.encode (A.toJSON peer)) `shouldSatisfy` (> 0)

  describe "Parsers" $ do
    it "refuse a path of an xpub that is not one, instead of failing later" $ do
      key <- generate (arbitraryXPubKey ctx)
      let good = marshalValue (btc, ctx) BinfoXPubPath {key = key, deriv = Deriv :/ 0 :/ 1}
          parse v = A.parseEither (unmarshalValue (btc, ctx)) v :: Either String BinfoXPubPath
      parse good `shouldSatisfy` (not . failed)
      forM_ ["M/0'/1", "0/1", "nonsense"] $ \path ->
        evaluate (failed (parse (with "path" (A.String path) good))) `shouldReturn` True
    it "read an error that they do not know, and one with an odd message" $ do
      let parse = A.parseEither A.parseJSON :: A.Value -> Either String Except
      parse (A.object ["error" A..= ("rate-limited" :: Text), "message" A..= ("slow down" :: Text)])
        `shouldBe` Right (StringError "rate-limited: slow down")
      parse (A.object ["error" A..= ("user-error" :: Text), "message" A..= (5 :: Int)])
        `shouldBe` Right (UserError "5")
    it "refuse bytes after a raw value" $ do
      tx <- generate (arbitraryTx btc ctx)
      let good = A.toJSON (RawResult tx)
          hex = case good of
            A.Object o | Just (A.String t) <- KeyMap.lookup "result" o -> t
            _ -> error "No result"
          parse v = A.parseEither A.parseJSON v :: Either String (RawResult Tx)
      parse good `shouldBe` Right (RawResult tx)
      parse (with "result" (A.String (hex <> "deadbeef")) good) `shouldSatisfy` failed
      (A.parseEither A.parseJSON (A.toJSON [hex <> "00"]) :: Either String (RawResultList Tx))
        `shouldSatisfy` failed
    it "refuse an address that is not text, instead of reading none" $ do
      unspent <- generate (arbitraryUnspent btc)
      let parse v = A.parseEither (unmarshalValue btc) v :: Either String Unspent
      parse (marshalValue btc unspent) `shouldBe` Right unspent
      parse (with "address" (A.Number 12345) (marshalValue btc unspent)) `shouldSatisfy` failed
      parse (with "address" A.Null (marshalValue btc unspent)) `shouldSatisfy` (not . failed)
    it "refuse a derivation type that they do not know" $
      (runGetS deserialize (B.singleton 9) :: Either String DeriveType) `shouldSatisfy` failed
    it "say what is missing of a place in the chain" $
      (A.parseEither A.parseJSON (A.object ["height" A..= (1 :: Int)]) :: Either String BlockRef)
        `shouldSatisfy` either ("height and position" `isInfixOf`) (const False)

  describe "Numbers" $ do
    it "of parameters are decimal digits that fit, and nothing else" $ do
      parseBinfoBlockId "7" `shouldBe` Just (BinfoBlockIndex 7)
      forM_ ["4294967296", "-1", "0x10", " 7", "7 ", "", "1e3"] $ \t ->
        (t, parseBinfoBlockId t) `shouldBe` (t, Nothing)
      parseBinfoTxId "18446744073709551616" `shouldBe` Nothing
      parseBinfoFilter " 0x6 " `shouldSatisfy` (not . isJust)
      parseBinfoFilter "6" `shouldSatisfy` isJust
      (parseParam btc ctx ["-1"] :: Maybe TimeParam) `shouldBe` Nothing
      (parseParam btc ctx ["18446744073709551616"] :: Maybe TimeParam) `shouldBe` Nothing
      (parseParam btc ctx ["1700000000"] :: Maybe TimeParam) `shouldBe` Just (TimeParam 1700000000)
      (parseParam btc ctx ["-5"] :: Maybe StartParam) `shouldBe` Nothing
      (parseParam btc ctx [" 5"] :: Maybe LimitParam) `shouldBe` Nothing
      (parseParam btc ctx ["0x10"] :: Maybe OffsetParam) `shouldBe` Nothing
      (parseParam btc ctx ["5"] :: Maybe LimitParam) `shouldBe` Just (LimitParam 5)
    it "of a date before 1970 are refused" $ do
      parseBinfoDate "01-01-1960" `shouldBe` Nothing
      parseBinfoDate "01-01-2020" `shouldBe` Just (BinfoDate 1577836800)
    it "do not wrap around when one is taken from a smaller one" $ do
      td <- generate (arbitraryTxData ctx)
      txDataFee td {prevs = IntMap.empty} `shouldBe` 0
      isOK BlockHealth {headers = 3000000000, blocks = 10, max = 2} `shouldBe` False

  describe "Order" $
    it "of xpub paths has no two different ones equal" $ do
      key <- generate (arbitraryXPubKey ctx)
      let a = BinfoXPubPath {key = key, deriv = Deriv :/ 0}
          b = BinfoXPubPath {key = key {index = key.index + 1}, deriv = Deriv :/ 0}
      (a == b, compare a b == EQ) `shouldBe` (False, False)

  describe "Client" $ do
    let nowhere = "http://127.0.0.1:1/"
        segwit = fromJust (textToAddr btc "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4")
        reason (Left (StringError why)) = why
        reason _ = ""
    it "answers an error for an address that the network cannot write" $
      apiCall ctx (ApiConfig bch nowhere) (GetAddrBalance segwit) `shouldReturn` Left (UserError "Invalid Param")
    it "answers an error, and throws nothing, when there is no server" $ do
      r <- apiCall ctx (ApiConfig btc nowhere) (GetAddrBalance segwit)
      reason r `shouldSatisfy` ("Could not reach the server" `isPrefixOf`)
      e <- apiEvents (ApiConfig btc nowhere) (\_ -> return True)
      reason e `shouldSatisfy` ("Could not reach the server" `isPrefixOf`)
    it "ends a batch of size zero" $ do
      r <- apiBatch ctx 0 (ApiConfig btc nowhere) (GetTxs [TxHash (sha256 ("a" :: B.ByteString))])
      reason r `shouldSatisfy` ("Could not reach the server" `isPrefixOf`)
    it "reads an error that comes in pieces" $ do
      let body = BL.toStrict (A.encode ThingNotFound)
          (one, two) = B.splitAt 10 body
      r <- serving [chunked "404 Not Found", chunk one, chunk two, "0\r\n\r\n"] $ \host ->
        apiCall ctx (ApiConfig btc host) (GetAddrBalance segwit)
      r `shouldBe` Left ThingNotFound
    it "says the status and the start of an answer that is not from the server" $ do
      let page = "<html>Bad Gateway</html>"
          answer =
            "HTTP/1.1 502 Bad Gateway\r\nContent-Length: "
              <> C.pack (show (B.length page))
              <> "\r\n\r\n"
              <> page
      r <- serving [answer] $ \host -> apiCall ctx (ApiConfig btc host) (GetAddrBalance segwit)
      r `shouldBe` Left (StringError "HTTP 502: <html>Bad Gateway</html>")
    it "goes on after a line of events that it does not know" $ do
      let one = TxHash (sha256 ("one" :: B.ByteString))
          two = TxHash (sha256 ("two" :: B.ByteString))
          line e = BL.toStrict (A.encode e) <> "\n"
          (start, end) = B.splitAt 20 (line (EventTx two))
      heard <- newIORef []
      r <- serving
        [ chunked "200 OK",
          chunk (line (EventTx one) <> "{\"type\":\"reorg\",\"id\":\"00\"}\n" <> start),
          chunk end,
          "0\r\n\r\n"
        ]
        $ \host -> apiEvents (ApiConfig btc host) (\e -> modifyIORef' heard (<> [e]) >> return True)
      r `shouldBe` Right ()
      readIORef heard `shouldReturn` [EventTx one, EventTx two]
