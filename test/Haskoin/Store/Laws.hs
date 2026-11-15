-- |
-- Laws that every encoding of the data of the store must obey, as specs that
-- take a generator. One line per type in a test table, instead of a block of
-- near-identical code per type and encoding.
--
-- Types whose encoding depends on the network get a random network for each
-- case ('anyNet'), so that every network is covered without running each law
-- once per network: the networks differ only in their address prefixes.
module Haskoin.Store.Laws
  ( anyNet,
    binaryLaws,
    jsonLaws,
    marshalLaws,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as A
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (parseMaybe)
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial (..))
import Data.Proxy (Proxy (..))
import Data.Serialize (Serialize)
import Data.Serialize qualified as Cereal
import Data.Typeable (Typeable, typeRep)
import Haskoin (Network (..), allNets)
import Haskoin.Util (MarshalJSON (..), decodeSerial, encodeSerial)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, choose, counterexample, elements, forAll, oneof, (.&&.), (===))

-- | Name of the type of a generator, for the description of a test.
name :: forall a. (Typeable a) => Gen a -> String
name _ = show (typeRep (Proxy @a))

-- | The value comes back from its bytes, and the three binary classes
-- ('Serial', cereal and binary) agree on the bytes, as do 'encodeSerial' and
-- 'decodeSerial'.
--
-- The reference is the 'Serial' instance run through cereal with 'runPutS'
-- and 'runGetS'. The cereal instances and the two functions do not run it
-- that way: they write and read on a buffer directly, for speed. So the
-- law also gives cereal bytes that are cut short, changed or followed by
-- others, and wants the result of the reference for them, error included.
binaryLaws :: forall a. (Eq a, Show a, Typeable a, Serial a, Serialize a, Binary a) => Gen a -> Spec
binaryLaws g = prop (name g) . forAll g $ \x ->
  let bytes = runPutS (serialize x)
      reference :: ByteString -> Either String a
      reference = runGetS deserialize
   in reference bytes === Right x
        .&&. Cereal.encode x === bytes
        .&&. BL.toStrict (Binary.encode x) === bytes
        .&&. encodeSerial x === bytes
        .&&. Cereal.decode bytes === Right x
        .&&. Binary.decode (BL.fromStrict bytes) === x
        .&&. decodeSerial bytes === Right x
        .&&. forAll (damaged bytes) (\bad -> Cereal.decode bad === reference bad)

-- | Bytes that are not what was written: cut short, with a byte changed, or
-- with a byte after them.
damaged :: ByteString -> Gen ByteString
damaged bytes =
  oneof
    [ (`B.take` bytes) <$> choose (0, B.length bytes),
      change <$> choose (0, max 0 (B.length bytes - 1)) <*> arbitrary,
      B.snoc bytes <$> arbitrary
    ]
  where
    change i w = B.take i bytes <> B.pack [w | i < B.length bytes] <> B.drop (i + 1) bytes

-- | The value comes back from its JSON, and the fast encoder ('A.toEncoding')
-- writes the same JSON as 'A.toJSON': the two are written by hand and could
-- drift apart.
jsonLaws :: forall a. (Eq a, Show a, Typeable a, ToJSON a, FromJSON a) => Gen a -> Spec
jsonLaws g = prop (name g) . forAll g $ \x ->
  A.fromJSON (A.toJSON x) === A.Success x
    .&&. (A.decode (A.encode x) :: Maybe Value) === Just (A.toJSON x)

-- | A value of a random network.
anyNet :: (Network -> Gen a) -> Gen a
anyNet g = elements allNets >>= g

-- | Like 'jsonLaws', for JSON that depends on the network, which is random
-- for each case.
marshalLaws ::
  forall a.
  (Eq a, Show a, Typeable a, MarshalJSON Network a) =>
  (Network -> Gen a) ->
  Spec
marshalLaws g = prop (name (g undefined)) . forAll (elements allNets) $ \net ->
  counterexample net.name . forAll (g net) $ \x ->
    parseMaybe (unmarshalValue net) (marshalValue net x) === Just x
      .&&. (A.decode (encodingToLazyByteString (marshalEncoding net x)) :: Maybe Value)
        === Just (marshalValue net x)
