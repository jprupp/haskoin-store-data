{-# OPTIONS_GHC -Wno-orphans #-}

module Haskoin.Store.WebCommonSpec (spec) where

import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Haskoin.Store.Data (DeriveType (..), SerialList (..), TxRef)
import Haskoin.Store.Gen (arbitraryDeriveType, arbitraryTransaction)
import Haskoin.Store.WebCommon
import Haskoin.Util.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = prepareContext $ \ctx -> do
  -- each case on a random network: they differ in their address prefixes
  describe "Parameters" $ do
    roundTrip ctx arbitraryAddress
    roundTrip ctx (listOf . arbitraryAddress)
    roundTrip ctx (const (arbitrary :: Gen StartParam))
    roundTrip ctx (const (arbitrarySizedNatural :: Gen OffsetParam))
    roundTrip ctx (const (arbitrarySizedNatural :: Gen LimitParam))
    roundTrip ctx (const (arbitrarySizedNatural :: Gen HeightParam))
    roundTrip ctx (const (arbitrary :: Gen HeightsParam))
    roundTrip ctx (const (arbitrarySizedNatural :: Gen TimeParam))
    roundTrip ctx (const (arbitraryXPubKey ctx))
    roundTrip ctx arbitraryDeriveType
    roundTrip ctx (const (NoCache <$> arbitrary))
    roundTrip ctx (const (NoTx <$> arbitrary))
    roundTrip ctx (const (listOf arbitraryBlockHash))
    roundTrip ctx (const (listOf arbitraryTxHash))

  -- the client reads what the server writes: lists in the JSON of the
  -- network, and the rest in plain JSON
  describe "Responses" $ do
    prop "a list of transactions" . forAll (elements allNets) $ \net ->
      forAll (listOf (arbitraryTransaction net)) $ \xs ->
        A.parseEither (parseResponse net ctx) (A.toJSON (map (marshalValue net) xs))
          === Right (SerialList xs)
    prop "a list of references" . forAll (listOf arbitrary) $ \xs ->
      A.parseEither (parseResponse btc ctx) (A.toJSON (xs :: [TxRef]))
        === Right (SerialList xs)

  describe "Start of a page" $ do
    it "is a transaction hash, a height up to 1230768000, or a later time" $ do
      parse ["1230768000"] `shouldBe` Just (StartParamHeight 1230768000)
      parse ["1230768001"] `shouldBe` Just (StartParamTime 1230768001)
      parse [txHashToHex z] `shouldBe` Just (StartParamHash z.get)
    it "refuses anything else" $
      parse ["yesterday"] `shouldBe` Nothing

  describe "Derivation" $
    it "is only normal on networks without SegWit" $ do
      encodeParam bch ctx DeriveP2WPKH `shouldBe` Nothing
      encodeParam btc ctx DeriveP2WPKH `shouldBe` Just ["segwit"]

  describe "Paths" $ do
    let addr = fromJust (textToAddr btc "1111111111111111111114oLvT2")
        path :: (ApiResource a b) => a -> String
        path = cs . queryPath btc ctx
    it "of a block" $ do
      path (GetBlockBest (NoTx False)) `shouldBe` "/block/best"
      path (GetBlockHeight (HeightParam 5) (NoTx False)) `shouldBe` "/block/height/5"
    it "of a transaction" $
      path (GetTx z) `shouldBe` "/transaction/" <> cs (txHashToHex z)
    it "of an address" $
      path (GetAddrBalance addr) `shouldBe` "/address/1111111111111111111114oLvT2/balance"
    prop "of an xpub" $
      forAll (arbitraryXPubKey ctx) $ \x ->
        path (GetXPubBalances x DeriveNormal (NoCache False))
          `shouldBe` "/xpub/" <> cs (xPubExport btc ctx x) <> "/balances"
  where
    z = TxHash (sha256 B.empty)
    parse = parseParam btc undefined :: [Text] -> Maybe StartParam

-- | A parameter comes back from its encoding in a URL, on a random network.
roundTrip :: forall a. (Eq a, Show a, Param a) => Ctx -> (Network -> Gen a) -> Spec
roundTrip ctx g =
  prop (cs (proxyLabel (Proxy :: Proxy a))) . forAll (elements allNets) $ \net ->
    counterexample net.name . forAll (g net) $ \p ->
      (encodeParam net ctx p >>= parseParam net ctx) `shouldBe` Just p

instance Arbitrary StartParam where
  arbitrary =
    oneof
      [ StartParamHash <$> arbitraryHash256,
        StartParamHeight . fromIntegral <$> (choose (0, 1230768000) :: Gen Word64),
        StartParamTime <$> choose (1230768001, maxBound)
      ]

instance Arbitrary HeightsParam where
  arbitrary = HeightsParam <$> listOf arbitrarySizedNatural
