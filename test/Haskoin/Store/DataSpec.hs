{-# LANGUAGE RecordWildCards #-}

module Haskoin.Store.DataSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial, serialize)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Store.Data
import Haskoin.Store.Gen
import Haskoin.Store.Laws
import Haskoin.Util.Arbitrary
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  -- only the types that the server keeps in its database and caches have
  -- a binary encoding: the API speaks JSON
  describe "Binary encoding" $ do
    binaryLaws (arbitrary :: Gen BlockRef)
    binaryLaws (arbitrary :: Gen TxRef)
    binaryLaws (arbitrary :: Gen BlockData)
    binaryLaws (arbitrary :: Gen Spender)
    binaryLaws (arbitrary :: Gen Prev)
    binaryLaws arbitraryTxData
    -- also covered inside the types below, but each has its own cereal and
    -- binary instances
    binaryLaws (anyNet arbitraryBalance)
    binaryLaws (anyNet arbitraryTokenBalance)
    binaryLaws (anyNet arbitraryUnspent)
    binaryLaws (anyNet arbitraryDeriveType)
    binaryLaws (anyNet arbitraryXPubSpec)
    binaryLaws (anyNet arbitraryXPubBal)

  describe "JSON" $ do
    jsonLaws (arbitrary :: Gen TxRef)
    jsonLaws (arbitrary :: Gen BlockRef)
    jsonLaws (arbitrary :: Gen Spender)
    jsonLaws (arbitrary :: Gen XPubSummary)
    jsonLaws (arbitrary :: Gen HealthCheck)
    jsonLaws (arbitrary :: Gen Event)
    jsonLaws (arbitrary :: Gen TxId)
    jsonLaws (arbitrary :: Gen PeerInfo)
    jsonLaws (arbitrary :: Gen (GenericResult XPubSummary))
    jsonLaws (arbitrary :: Gen (RawResult BlockData))
    jsonLaws (arbitrary :: Gen (RawResultList BlockData))
    jsonLaws (arbitrary :: Gen Except)
    jsonLaws (arbitrary :: Gen BinfoWallet)
    jsonLaws (arbitrary :: Gen BinfoSymbol)
    jsonLaws (arbitrary :: Gen BinfoBlockInfo)
    jsonLaws (arbitrary :: Gen BinfoInfo)
    jsonLaws (arbitrary :: Gen BinfoSpender)
    jsonLaws (arbitrary :: Gen BinfoRate)
    jsonLaws (arbitrary :: Gen BinfoTicker)
    jsonLaws (arbitrary :: Gen BinfoTxId)
    jsonLaws (arbitrary :: Gen BinfoShortBal)
    jsonLaws (arbitrary :: Gen BinfoHistory)
    jsonLaws (arbitrary :: Gen BinfoHeader)
    jsonLaws (arbitrary :: Gen BinfoBlockInfos)

  -- Balance, Unspent, StoreInput, StoreOutput and the inputs and outputs of
  -- BinfoTx are encoded through their own instances inside these.
  describe "JSON of a network" $ do
    marshalLaws arbitraryXPubBal
    marshalLaws arbitraryTokenBalance
    marshalLaws arbitraryXPubUnspent
    marshalLaws arbitraryBlockData
    marshalLaws arbitraryTransaction
    marshalLaws arbitraryEventCommand
    marshalLaws arbitraryBinfoMultiAddr
    marshalLaws arbitraryBinfoBalance
    marshalLaws arbitraryBinfoBlock
    marshalLaws arbitraryBinfoTx
    marshalLaws arbitraryBinfoXPubPath
    marshalLaws arbitraryBinfoUnspent
    marshalLaws arbitraryBinfoRawAddr
    marshalLaws arbitraryBinfoMempool

  describe "Health check" $ do
    let bare =
          HealthCheck
            { blocks = BlockHealth 10 10 2,
              lastBlock = TimeHealth 5 100,
              lastTx = TimeHealth 5 100,
              pendingTxs = MaxHealth 0 100,
              peers = CountHealth 3 1,
              network = "btc",
              version = "1.9.0",
              time = 1700000000,
              redis = Nothing,
              database = Nothing,
              mempool = Nothing
            }
        database =
          DatabaseHealth
            { writes = WritesFlowing,
              pendingCompaction = 0,
              delayedWriteRate = 0,
              compactions = 0,
              flushes = 1,
              memtables = 0,
              size = 70000000000,
              liveData = 69000000000,
              errors = 0
            }
        text key = \case
          A.Object o ->
            KeyMap.lookup key o >>= \case
              A.Object c -> KeyMap.lookup "text" c
              _ -> Nothing
          _ -> Nothing
        without key = \case
          A.Object o -> A.Object (KeyMap.delete key o)
          v -> v
    it "reads what a store wrote before there was a check of Redis" $ do
      A.fromJSON (without "redis" (A.toJSON bare {redis = Just False})) `shouldBe` A.Success bare
    it "refuses a time that it cannot read, instead of failing when the time is used" $ do
      let badly = \case
            A.Object o -> A.Object (KeyMap.insert "time" (A.String "yesterday") o)
            v -> v
      (A.fromJSON (badly (A.toJSON bare)) :: A.Result HealthCheck) `shouldSatisfy` \case
        A.Error _ -> True
        A.Success _ -> False
    it "says in words what the whole store and each check found" $ do
      let json = A.toJSON bare {database = Just database, redis = Just True}
      json `shouldSatisfy` \case
        A.Object o ->
          KeyMap.lookup "text" o
            == Just
              ( A.String
                  "The store is healthy: it has every block up to height 10, \
                  \with 3 peers connected, and its Redis answers."
              )
        _ -> False
      text "blocks" json `shouldBe` Just (A.String "The database has every block up to the best header, at height 10.")
      text "peers" json `shouldBe` Just (A.String "3 peers connected, and at least 1 needed.")
      text "redis" json `shouldBe` Just (A.String "Redis answers.")
      text "database" json
        `shouldBe` Just
          ( A.String . T.unwords $
              [ "RocksDB writes at full speed.",
                "Its files hold 70.0 GB, 69.0 GB of them live data.",
                "1 flush running."
              ]
          )
    it "says what is wrong with a store that is not healthy, and nothing else" $ do
      let sick = bare {blocks = BlockHealth 100 10 2, database = Just database {writes = WritesStopped}}
      healthText sick
        `shouldBe` T.unwords
          [ "The store is not healthy.",
            "The database is 90 blocks behind the best header, at height 100, more than the 2 allowed:",
            "the store is syncing, or cannot import blocks fast enough.",
            "RocksDB stopped writes until compactions or flushes catch up: the store imports nothing.",
            "Its files hold 70.0 GB, 69.0 GB of them live data.",
            "1 flush running."
          ]
      isOK bare {database = Just database {writes = WritesStopped}} `shouldBe` False
      isOK bare {database = Just database {writes = WritesDelayed}} `shouldBe` True
    it "writes numbers as people read them, and leaves out the work that is nothing" $ do
      let busy = database {pendingCompaction = 1234567890123, compactions = 3, flushes = 0, memtables = 2}
      databaseText busy
        `shouldBe` T.unwords
          [ "RocksDB writes at full speed.",
            "Its files hold 70.0 GB, 69.0 GB of them live data.",
            "1.2 TB to compact, 3 compactions running, 2 full tables in memory to flush."
          ]
      databaseText database {size = 0, liveData = 0, flushes = 0}
        `shouldBe` "RocksDB writes at full speed. The database has written no files yet: \
                   \what it holds is in memory. Nothing waits to be compacted or flushed."
      pendingTxsText (MaxHealth 12345 10000)
        `shouldBe` "12,345 transactions asked of peers are still to come, more than the 10,000 allowed: \
                   \the peers do not send what the store asks for, or it cannot take it in fast enough."
      lastBlockText (TimeHealth (-24) 14400) `shouldBe` "The best header is new, within the 4 hours allowed."
    it "writes the example of the API reference of haskoin-store" $ do
      let example =
            HealthCheck
              { blocks = BlockHealth 688798 688794 2,
                lastBlock = TimeHealth 3279 7200,
                lastTx = TimeHealth 0 300,
                pendingTxs = MaxHealth 0 100,
                peers = CountHealth 0 1,
                network = "bch",
                version = "2.0.0",
                time = 1623666103,
                redis = Just True,
                database = Just database {size = 412000000000, liveData = 405300000000, flushes = 0},
                mempool = Just (MempoolHealth 1 False 0 0)
              }
      healthText example
        `shouldBe` T.unwords
          [ "The store is not healthy. The database is 4 blocks behind the best",
            "header, at height 688,798, more than the 2 allowed: the store is",
            "syncing, or cannot import blocks fast enough. 0 peers connected,",
            "fewer than the 1 needed: the store hears too little of the network."
          ]
      lastBlockText example.lastBlock `shouldBe` "The best header is 54 minutes old, within the 2 hours allowed."
      lastTxText example.lastTx `shouldBe` "A transaction or block came just now, within the 5 minutes allowed."
      pendingTxsText example.pendingTxs
        `shouldBe` "0 transactions asked of peers are still to come, within the 100 allowed."
      fmap databaseText example.database
        `shouldBe` Just
          "RocksDB writes at full speed. Its files hold 412.0 GB, 405.3 GB of \
          \them live data. Nothing waits to be compacted or flushed."
    it "says how the store stands with the mempool of its nodes, which is no check" $ do
      let alone = MempoolHealth {sources = 0, dirty = True, known = 3, held = 1}
          sourced = MempoolHealth {sources = 2, dirty = False, known = 0, held = 0}
      mempoolText sourced
        `shouldBe` "The store has 2 sources to read a whole mempool from again, \
                   \which it does when it may have missed a transaction."
      mempoolText sourced {dirty = True, sources = 1}
        `shouldBe` "The store has 1 source to read a whole mempool from again, \
                   \which it does when it may have missed a transaction. \
                   \It may have missed some, and is about to read again."
      mempoolText alone
        `shouldBe` "The store has nothing to read a whole mempool from again, \
                   \no peer that gives its own and no RPC of a node: \
                   \a transaction that it misses stays missed until a block has it. \
                   \It may have missed some. \
                   \3 transactions are on the way to the shared mempool, 1 held."
      -- a store with no source is healthy, and says what it lacks
      isOK bare {mempool = Just alone} `shouldBe` True
      healthText bare {mempool = Just alone}
        `shouldBe` "The store is healthy: it has every block up to height 10, with 3 peers connected. \
                   \The store has nothing to read a whole mempool from again, \
                   \no peer that gives its own and no RPC of a node: \
                   \a transaction that it misses stays missed until a block has it."
      A.fromJSON (without "mempool" (A.toJSON bare {mempool = Just alone})) `shouldBe` A.Success bare
    it "reads what a store wrote before the database had a check" $ do
      A.fromJSON (without "database" (A.toJSON bare {database = Just database})) `shouldBe` A.Success bare
    it "is fit to serve without its Redis, and says that it is without" $ do
      isOK bare {redis = Just False} `shouldBe` True
      A.toJSON bare {redis = Just False} `shouldSatisfy` \case
        A.Object o -> KeyMap.lookup "redis" o == Just (A.object ["ok" A..= False, "text" A..= redisText False])
        _ -> False
      A.toJSON bare `shouldSatisfy` \case
        A.Object o -> not (KeyMap.member "redis" o)
        _ -> False

  -- Other programs read these formats: they must not change by accident.
  describe "Wire format" $ do
    let txid = TxHash (sha256 B.empty)
        txidHex = "55b852781b9995a44c939b64e441ae2724b96f99c8f4fb9a141cfc9842c4b0e3"
        txidRaw = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        burn = fromJust (textToAddr btc "1111111111111111111114oLvT2")
    wire (BlockRef 100 2) "{\"height\":100,\"position\":2}" "01ffffff9bfffffffd"
    wire (MemRef 1700000000) "{\"mempool\":1700000000}" "00ffffffff9aac0eff"
    wire (Spender txid 3) ("{\"txid\":\"" <> txidHex <> "\",\"input\":3}") (txidRaw <> "00000003")
    wire
      (TxRef (BlockRef 100 2) txid)
      ("{\"block\":{\"height\":100,\"position\":2},\"txid\":\"" <> txidHex <> "\"}")
      ("01ffffff9bfffffffd" <> txidRaw)
    it "Balance" $
      marshalValue btc (Balance burn 1 2 3 4 5)
        `shouldBe` json
          "{\"address\":\"1111111111111111111114oLvT2\",\"confirmed\":1,\
          \\"unconfirmed\":2,\"utxo\":3,\"txs\":4,\"received\":5}"
    it "XPubSummary" $
      A.toJSON XPubSummary {confirmed = 1, unconfirmed = 2, received = 3, utxo = 4, change = 5, external = 6}
        `shouldBe` json
          "{\"balance\":{\"confirmed\":1,\"unconfirmed\":2,\"received\":3,\"utxo\":4},\
          \\"indices\":{\"change\":5,\"external\":6}}"

  describe "Blockchain.info helpers" $ do
    it "reads its request parameters" $ do
      let h = TxHash (sha256 B.empty)
          b = BlockHash (sha256 B.empty)
      parseBinfoTxId (txHashToHex h) `shouldBe` Just (BinfoTxIdHash h)
      parseBinfoTxId "8011147086339971" `shouldBe` Just (BinfoTxIdIndex 8011147086339971)
      parseBinfoTxId "nonsense" `shouldBe` Nothing
      parseBinfoBlockId (blockHashToHex b) `shouldBe` Just (BinfoBlockHash b)
      parseBinfoBlockId "100" `shouldBe` Just (BinfoBlockIndex 100)
      map parseBinfoFilter ["0", "1", "2", "3", "4", "5", "6", "7"]
        `shouldBe` [ Just BinfoFilterAll,
                     Just BinfoFilterSent,
                     Just BinfoFilterReceived,
                     Just BinfoFilterMoved,
                     Nothing,
                     Just BinfoFilterConfirmed,
                     Just BinfoFilterAll,
                     Just BinfoFilterMempool
                   ]
      map parseBinfoDate ["09-11-2026", "09/11/2026", "2026-11-09"]
        `shouldBe` [Just (BinfoDate 1794182400), Just (BinfoDate 1794182400), Nothing]
    it "numbers a transaction by the first bits of its hash" $ do
      let h = TxHash (sha256 B.empty)
      encodeBinfoTxId False h `shouldBe` BinfoTxIdHash h
      encodeBinfoTxId True h `shouldBe` BinfoTxIdIndex 8011147086339971
    it "writes values as signed big-endian hexadecimal" $
      map binfoHexValue [0, 1, 127, 128, 255, 256, 65535]
        `shouldBe` ["00", "01", "7f", "0080", "00ff", "0100", "00ffff"]
    it "reads addresses and xpubs separated by bars or commas" $
      forAll (vectorOf 3 (arbitraryAddress btc)) $ \as ->
        forAll (arbitraryXPubKey) $ \x ->
          let txt = mconcat [t <> sep | (a, sep) <- zip as ["|", ",", "|"], Just t <- [addrToText btc a]] <> xPubExport btc x
           in parseBinfoAddr btc txt `shouldBe` Just (map BinfoAddr as <> [BinfoXpub x])
    it "refuses anything that is neither an address nor an xpub" $
      parseBinfoAddr btc "nonsense" `shouldBe` Nothing
    it "reads nothing from nothing" $
      parseBinfoAddr btc "" `shouldBe` Just []
    it "gives the next indices of an xpub and counts only payments to it" $
      forAll (arbitraryXPubSpec btc) $ \x ->
        forAll (vectorOf 3 (arbitraryAddress btc)) $ \as -> case as of
          [a1, a2, a3] ->
            let bal a r c = (zeroBalance a) {received = r, confirmed = c, txs = 1}
                xbals =
                  [ XPubBal [0, 4] (bal a1 100 60),
                    XPubBal [0, 1] (bal a2 50 0),
                    XPubBal [1, 2] (bal a3 30 30)
                  ]
             in case toBinfoAddrs HashMap.empty (HashMap.singleton x xbals) (HashMap.singleton x 7) of
                  [BinfoXPubBalance {..}] ->
                    (external, change, received, balance, sent, txs) `shouldBe` (5, 3, 150, 90, 60, 7)
                  bs -> expectationFailure ("not the balance of one xpub: " <> show bs)
          _ -> discard

  describe "Balances" $
    it "are null only when nothing ever touched the address" $
      forAll (arbitraryBalance btc) $ \b ->
        nullBalance b
          === (b.confirmed == 0 && b.unconfirmed == 0 && b.utxo == 0 && b.txs == 0 && b.received == 0)
          .&&. nullBalance (zeroBalance b.address)

  describe "Tokens" $ do
    it "are read from the script field on Bitcoin Cash only" $
      forAll arbitraryTokenData $ \t ->
        forAll arbitraryBS1 $ \lock ->
          let script = maybe B.empty (<> lock) (encodeTokenPrefix t)
           in (scriptTokens bch script, scriptTokens btc script, scriptTokens bch lock)
                === (Just t, Nothing, Nothing)
    it "are in the JSON of an output on Bitcoin Cash, with null for none" $
      forAll arbitraryTokenData $ \t ->
        let script = fromMaybe B.empty (encodeTokenPrefix t) <> "\x51"
            out s = StoreOutput {value = 1, script = s, spender = Nothing, address = Nothing}
            field net s = case marshalValue net (out s) of
              A.Object o -> KeyMap.lookup "token" o
              _ -> Nothing
         in (field bch script, field bch "\x51", field btc script)
              === (Just (A.toJSON (Just t)), Just A.Null, Nothing)

  describe "Derivation types" $
    it "are named legacy, compat and segwit, and read normal as legacy" $ do
      map deriveTypeToText [DeriveNormal, DeriveP2SH, DeriveP2WPKH]
        `shouldBe` ["legacy", "compat", "segwit"]
      map textToDeriveType ["legacy", "normal", "compat", "segwit", "other", "Legacy"]
        `shouldBe` map Just [DeriveNormal, DeriveNormal, DeriveP2SH, DeriveP2WPKH]
          <> [Nothing, Nothing]

  describe "Coin instance of Unspent" $
    it "reads the input kind from the output script" $
      property $
        forAll (arbitraryScriptOutput btc) $ \so ->
          case encodeOutput so of
            Left _ -> discard
            Right script ->
              let u =
                    Unspent
                      { block = MemRef 0,
                        outpoint = OutPoint (TxHash z) 0,
                        value = 1,
                        script = runPutS (serialize script),
                        address = Nothing
                      }
               in coinInputKind u `shouldBe` expectedKind so
  describe "toTransaction" $ do
    -- The JSON of the only output of a transaction that pays 1000 satoshi
    -- to a script given in hex.
    let outputJson net scriptHex =
          let script = fromJust (decodeHex scriptHex)
              tx = Tx 2 [TxIn (OutPoint (TxHash z) 0) "" maxBound] [TxOut 1000 script] [] 0
              td =
                TxData
                  { block = MemRef 0,
                    tx = tx,
                    prevs = IntMap.empty,
                    deleted = False,
                    rbf = False,
                    timestamp = 0,
                    spenders = IntMap.empty
                  }
           in map (marshalValue net) (toTransaction net td).outputs
        -- Pay to witness public key hash, from the examples of BIP 173.
        p2wpkh = "0014751e76e8199196d454941c45d1b3a323f1433bd6"
        -- OP_HASH256, a push of 32 bytes, OP_EQUAL.
        hashLock = "aa20bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb87"
    it "computes the id, size, weight and witnesses of the transaction" $
      forAll (arbitraryTxData) $ \td ->
        let t = toTransaction btc td
            len :: Tx -> Int
            len = B.length . runPutS . serialize
            witless = let Tx {..} = td.tx in Tx {witness = [], ..}
            ws = take (length td.tx.inputs) (td.tx.witness <> repeat [])
         in (t.txid, t.size, t.weight, map (.witness) t.inputs)
              `shouldBe` ( txHash td.tx,
                           fromIntegral (len td.tx),
                           fromIntegral (len witless * 3 + len td.tx),
                           ws
                         )
    it "gives the same with the id given" $
      forAll (arbitraryTxData) $ \td ->
        toTransactionWithId btc (txHash td.tx) td `shouldBe` toTransaction btc td
    it "counts sizes as serializing does, with large counts and pushes" $
      -- counts and lengths of 253 and more take more than a byte
      forAll (arbitraryTxData) $ \td -> forAll (choose (0, 3)) $ \k ->
        let big = B.replicate (0xfc + k * 0x100) 0x51
            Tx {..} = td.tx
            grown = case outputs of
              TxOut v _ : os -> Tx {outputs = TxOut v big : os, ..}
              [] -> Tx {..}
            t = toTransaction btc (td {tx = grown} :: TxData)
         in t.size `shouldBe` fromIntegral (B.length (runPutS (serialize grown)))
    -- an output has an address only where the network has that kind
    forM_
      [ ("a SegWit output an address on Bitcoin", btc, p2wpkh, "\"bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4\"", ""),
        ("a SegWit-shaped output no address on Bitcoin Cash", bch, p2wpkh, "null", ",\"token\":null"),
        ("a 256-bit hash lock no address on Bitcoin", btc, hashLock, "null", "")
      ]
      $ \(what, net, script, addr, token) ->
        it ("gives " <> what) $
          outputJson net script
            `shouldBe` [ json
                           ( "{\"address\":"
                               <> addr
                               <> ",\"pkscript\":\""
                               <> BL.fromStrict (encodeUtf8 script)
                               <> "\",\"value\":1000,\"spent\":false,\"spender\":null"
                               <> token
                               <> "}"
                           )
                       ]
  where
    z = sha256 B.empty
    expectedKind = \case
      PayPK {} -> InputPK
      PayPKHash {} -> InputPKHash
      PayMulSig {keys, required} -> InputMulSig required (length keys)
      PayScriptHash {} -> InputNestedWitnessPKHash
      PayWitnessPKHash {} -> InputWitnessPKHash
      PayWitnessScriptHash {} -> InputWitnessMulSig 1 1
      PayWitness 1 bs | B.length bs == 32 -> InputTaproot
      _ -> InputPKHash

-- | A value has this JSON and these bytes.
wire :: (Show a, A.ToJSON a, Serial a) => a -> BL.ByteString -> Text -> Spec
wire x j h =
  it (takeWhile (/= ' ') (show x)) $ do
    A.toJSON x `shouldBe` json j
    encodeHex (runPutS (serialize x)) `shouldBe` h

-- | Read a JSON literal.
json :: BL.ByteString -> Value
json = fromJust . A.decode
