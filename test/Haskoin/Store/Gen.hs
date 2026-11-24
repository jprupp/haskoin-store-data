{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Generators of the data of the store, for property tests. Collections of
-- large values (transactions, blocks, inputs, outputs) are kept to three
-- elements: a round trip of one element tests the encoding as well as a round
-- trip of a hundred, and big values made the tests take minutes.
module Haskoin.Store.Gen
  ( small,
    small1,
    arbitraryTxData,
    arbitraryStoreInput,
    arbitraryStoreOutput,
    arbitraryTransaction,
    arbitraryXPubSpec,
    arbitraryEventCommand,
    arbitraryDeriveType,
    arbitraryBalance,
    arbitraryTokenBalance,
    arbitraryUnspent,
    arbitraryBlockData,
    arbitraryXPubBal,
    arbitraryXPubUnspent,
    arbitraryBinfoMultiAddr,
    arbitraryBinfoRawAddr,
    arbitraryBinfoBalance,
    arbitraryBinfoBlock,
    arbitraryBinfoTx,
    arbitraryBinfoTxInput,
    arbitraryBinfoTxOutput,
    arbitraryBinfoXPubPath,
    arbitraryBinfoUnspent,
    arbitraryBinfoUnspents,
    arbitraryBinfoMempool,
  )
where

import Data.ByteString qualified as B
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Haskoin
import Haskoin.Store.Data
import Haskoin.Util.Arbitrary
import Test.QuickCheck

-- | Up to three values.
small :: Gen a -> Gen [a]
small g = choose (0, 3) >>= (`vectorOf` g)

-- | One to three values.
small1 :: Gen a -> Gen [a]
small1 g = choose (1, 3) >>= (`vectorOf` g)

instance Arbitrary BlockRef where
  arbitrary =
    oneof [BlockRef <$> arbitrary <*> arbitrary, MemRef <$> arbitrary]

instance Arbitrary Prev where
  arbitrary = Prev <$> arbitraryBS1 <*> arbitrary

arbitraryTxData :: Gen TxData
arbitraryTxData =
  TxData
    <$> arbitrary
    <*> arbitraryTx btc
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

arbitraryStoreInput :: Network -> Gen StoreInput
arbitraryStoreInput net = do
  store <-
    oneof
      [ StoreCoinbase
          <$> arbitraryOutPoint
          <*> arbitrary
          <*> arbitraryBS1
          <*> listOf arbitraryBS1,
        StoreInput
          <$> arbitraryOutPoint
          <*> arbitrary
          <*> arbitraryBS1
          <*> arbitraryBS1
          <*> arbitrary
          <*> listOf arbitraryBS1
          <*> arbitraryMaybe (arbitraryAddress net)
      ]
  let res
        | net.segWit = store
        | otherwise = witless store
  return res
  where
    witless StoreInput {..} = StoreInput {witness = [], ..}
    witless StoreCoinbase {..} = StoreCoinbase {witness = [], ..}

instance Arbitrary Spender where
  arbitrary = Spender <$> arbitraryTxHash <*> arbitrary

arbitraryStoreOutput :: Network -> Gen StoreOutput
arbitraryStoreOutput net =
  StoreOutput
    <$> arbitrary
    <*> arbitraryBS1
    <*> arbitrary
    <*> arbitraryMaybe (arbitraryAddress net)

arbitraryTransaction :: Network -> Gen Transaction
arbitraryTransaction net =
  Transaction
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> small1 (arbitraryStoreInput net)
    <*> small1 (arbitraryStoreOutput net)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitraryTxHash
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary PeerInfo where
  arbitrary =
    PeerInfo
      <$> (cs <$> listOf arbitraryUnicodeChar)
      <*> listOf arbitraryPrintableChar
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary BlockHealth where
  arbitrary =
    BlockHealth
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary TimeHealth where
  arbitrary =
    TimeHealth
      <$> arbitrary
      <*> arbitrary

instance Arbitrary CountHealth where
  arbitrary =
    CountHealth
      <$> arbitrary
      <*> arbitrary

instance Arbitrary MaxHealth where
  arbitrary =
    MaxHealth
      <$> arbitrary
      <*> arbitrary

instance Arbitrary DatabaseWrites where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DatabaseHealth where
  arbitrary =
    DatabaseHealth
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary MempoolHealth where
  arbitrary = MempoolHealth <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HealthCheck where
  arbitrary =
    HealthCheck
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary RejectCode where
  arbitrary =
    elements
      [ RejectMalformed,
        RejectInvalid,
        RejectObsolete,
        RejectDuplicate,
        RejectNonStandard,
        RejectDust,
        RejectInsufficientFee,
        RejectCheckpoint
      ]

arbitraryXPubSpec :: Network -> Gen XPubSpec
arbitraryXPubSpec net = XPubSpec <$> arbitraryXPubKey <*> arbitraryDeriveType net

arbitraryDeriveType :: Network -> Gen DeriveType
arbitraryDeriveType net =
  if net.segWit
    then elements [DeriveNormal, DeriveP2SH, DeriveP2WPKH]
    else return DeriveNormal

instance Arbitrary TxId where
  arbitrary = TxId <$> arbitraryTxHash

instance Arbitrary TxRef where
  arbitrary = TxRef <$> arbitrary <*> arbitraryTxHash

arbitraryBalance :: Network -> Gen Balance
arbitraryBalance net =
  Balance
    <$> arbitraryAddress net
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

arbitraryTokenBalance :: Network -> Gen TokenBalance
arbitraryTokenBalance net =
  TokenBalance
    <$> arbitraryAddress net
    <*> arbitraryTxHash
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

arbitraryUnspent :: Network -> Gen Unspent
arbitraryUnspent net =
  Unspent
    <$> arbitrary
    <*> arbitraryOutPoint
    <*> arbitrary
    <*> arbitraryBS1
    <*> arbitraryMaybe (arbitraryAddress net)

instance Arbitrary BlockData where
  arbitrary =
    BlockData
      <$> arbitrary
      <*> arbitrary
      <*> (fromInteger <$> suchThat arbitrary (0 <=))
      <*> arbitraryBlockHeader
      <*> arbitrary
      <*> arbitrary
      <*> listOf1 arbitraryTxHash
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

arbitraryBlockData :: Network -> Gen BlockData
arbitraryBlockData net = do
  dat@BlockData {..} <- arbitrary
  return $ if net.segWit then dat else BlockData {weight = 0, ..}

instance (Arbitrary a) => Arbitrary (GenericResult a) where
  arbitrary = GenericResult <$> arbitrary

instance (Arbitrary a) => Arbitrary (RawResult a) where
  arbitrary = RawResult <$> arbitrary

instance (Arbitrary a) => Arbitrary (RawResultList a) where
  arbitrary = RawResultList <$> arbitrary

arbitraryXPubBal :: Network -> Gen XPubBal
arbitraryXPubBal net = XPubBal <$> arbitrary <*> arbitraryBalance net

arbitraryXPubUnspent :: Network -> Gen XPubUnspent
arbitraryXPubUnspent net = XPubUnspent <$> arbitraryUnspent net <*> arbitrary

instance Arbitrary XPubSummary where
  arbitrary =
    XPubSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Event where
  arbitrary =
    oneof
      [ EventBlock <$> arbitraryBlockHash,
        EventTx <$> arbitraryTxHash
      ]

-- | A command for the events of a network, of every kind.
arbitraryEventCommand :: Network -> Gen EventCommand
arbitraryEventCommand net =
  oneof
    [ EventWatch <$> listOf (arbitraryAddress net),
      EventUnwatch <$> listOf (arbitraryAddress net),
      EventWatchXPub <$> arbitraryXPubSpec net,
      EventUnwatchXPub <$> arbitraryXPubSpec net,
      EventFilterLoad <$> arbitraryBloomFilter,
      EventFilterAdd . B.pack <$> (choose (0, maxFilterAddSize) >>= vector),
      return EventFilterClear,
      return EventAll,
      return EventNone
    ]

instance Arbitrary Except where
  arbitrary =
    oneof
      [ return ThingNotFound,
        return ServerError,
        return BadRequest,
        UserError <$> arbitrary,
        StringError <$> arbitrary,
        TxIndexConflict <$> listOf1 arbitraryTxHash,
        return ServerTimeout,
        return RequestTooLarge,
        return BehindFleet
      ]

---------------------------------------
-- Blockchain.info API Compatibility --
---------------------------------------

instance Arbitrary BinfoTxId where
  arbitrary =
    oneof
      [ BinfoTxIdHash <$> arbitraryTxHash,
        BinfoTxIdIndex <$> arbitrary
      ]

arbitraryBinfoMultiAddr :: Network -> Gen BinfoMultiAddr
arbitraryBinfoMultiAddr net = do
  b <- arbitraryBinfoBalance net
  let addresses = [b]
  wallet <- arbitrary
  txs <- small (arbitraryBinfoTx net)
  info <- arbitrary
  recommendFee <- arbitrary
  let cashAddr = isJust net.cashAddrPrefix
  return BinfoMultiAddr {..}

arbitraryBinfoRawAddr :: Network -> Gen BinfoRawAddr
arbitraryBinfoRawAddr net = do
  address <-
    oneof
      [ BinfoAddr <$> arbitraryAddress net,
        BinfoXpub <$> arbitraryXPubKey
      ]
  balance <- arbitrary
  ntx <- arbitrary
  utxo <- arbitrary
  received <- arbitrary
  sent <- arbitrary
  txs <- small (arbitraryBinfoTx net)
  return $ BinfoRawAddr {..}

instance Arbitrary BinfoShortBal where
  arbitrary = BinfoShortBal <$> arbitrary <*> arbitrary <*> arbitrary

arbitraryBinfoBalance :: Network -> Gen BinfoBalance
arbitraryBinfoBalance net = do
  address <- arbitraryAddress net
  txs <- arbitrary
  received <- arbitrary
  sent <- arbitrary
  balance <- arbitrary
  xpub <- arbitraryXPubKey
  external <- arbitrary
  change <- arbitrary
  elements [BinfoAddrBalance {..}, BinfoXPubBalance {..}]

instance Arbitrary BinfoWallet where
  arbitrary = do
    balance <- arbitrary
    txs <- arbitrary
    filtered <- arbitrary
    received <- arbitrary
    sent <- arbitrary
    return BinfoWallet {..}

arbitraryBinfoBlock :: Network -> Gen BinfoBlock
arbitraryBinfoBlock net = do
  hash <- arbitraryBlockHash
  version <- arbitrary
  prev <- arbitraryBlockHash
  merkle <- (.get) <$> arbitraryTxHash
  timestamp <- arbitrary
  bits <- arbitrary
  next <- small arbitraryBlockHash
  ntx <- arbitrary
  fee <- arbitrary
  nonce <- arbitrary
  size <- arbitrary
  index <- arbitrary
  main <- arbitrary
  height <- arbitrary
  weight <- arbitrary
  txs <- small (arbitraryBinfoTx net)
  return BinfoBlock {..}

arbitraryBinfoTx :: Network -> Gen BinfoTx
arbitraryBinfoTx net = do
  txid <- arbitraryTxHash
  version <- arbitrary
  inputs <- small1 (arbitraryBinfoTxInput net)
  outputs <- small1 (arbitraryBinfoTxOutput net)
  let inputCount = fromIntegral $ length inputs
      outputCount = fromIntegral $ length outputs
  size <- arbitrary
  weight <- arbitrary
  fee <- arbitrary
  relayed <- cs <$> listOf arbitraryUnicodeChar
  locktime <- arbitrary
  index <- arbitrary
  doubleSpend <- arbitrary
  rbf <- arbitrary
  timestamp <- arbitrary
  blockIndex <- arbitrary
  blockHeight <- arbitrary
  balance <- arbitrary
  return BinfoTx {..}

arbitraryBinfoTxInput :: Network -> Gen BinfoTxInput
arbitraryBinfoTxInput net = do
  sequence <- arbitrary
  witness <- B.pack <$> listOf arbitrary
  script <- B.pack <$> listOf arbitrary
  index <- arbitrary
  output <- arbitraryBinfoTxOutput net
  return BinfoTxInput {..}

arbitraryBinfoTxOutput :: Network -> Gen BinfoTxOutput
arbitraryBinfoTxOutput net = do
  typ <- arbitrary
  spent <- arbitrary
  value <- arbitrary
  index <- arbitrary
  txidx <- arbitrary
  script <- B.pack <$> listOf arbitrary
  spenders <- arbitrary
  address <- arbitraryMaybe (arbitraryAddress net)
  xpub <- arbitraryMaybe $ arbitraryBinfoXPubPath net
  return BinfoTxOutput {..}

instance Arbitrary BinfoSpender where
  arbitrary = do
    txidx <- arbitrary
    input <- arbitrary
    return BinfoSpender {..}

arbitraryBinfoXPubPath :: Network -> Gen BinfoXPubPath
arbitraryBinfoXPubPath _net = do
  key <- arbitraryXPubKey
  deriv <- arbitrarySoftPath
  return BinfoXPubPath {..}

instance Arbitrary BinfoInfo where
  arbitrary = do
    connected <- arbitrary
    conversion <- arbitrary
    fiat <- arbitrary
    crypto <- arbitrary
    head <- arbitrary
    return BinfoInfo {..}

instance Arbitrary BinfoBlockInfo where
  arbitrary = do
    hash <- arbitraryBlockHash
    height <- arbitrary
    timestamp <- arbitrary
    index <- arbitrary
    return BinfoBlockInfo {..}

instance Arbitrary BinfoSymbol where
  arbitrary = do
    code <- cs <$> listOf1 arbitraryUnicodeChar
    symbol <- cs <$> listOf1 arbitraryUnicodeChar
    name <- cs <$> listOf1 arbitraryUnicodeChar
    conversion <- arbitrary
    after <- arbitrary
    local <- arbitrary
    return BinfoSymbol {..}

instance Arbitrary BinfoRate where
  arbitrary = BinfoRate <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BinfoTicker where
  arbitrary = do
    fifteen <- arbitrary
    sell <- arbitrary
    buy <- arbitrary
    last <- arbitrary
    symbol <- cs <$> listOf1 arbitraryUnicodeChar
    return BinfoTicker {..}

instance Arbitrary BinfoHistory where
  arbitrary = do
    date <- cs <$> listOf1 arbitraryUnicodeChar
    time <- cs <$> listOf1 arbitraryUnicodeChar
    typ <- cs <$> listOf1 arbitraryUnicodeChar
    amount <- arbitrary
    valueThen <- arbitrary
    valueNow <- arbitrary
    rateThen <- arbitrary
    txid <- arbitraryTxHash
    fee <- arbitrary
    return BinfoHistory {..}

arbitraryBinfoUnspent :: Network -> Gen BinfoUnspent
arbitraryBinfoUnspent net = do
  txid <- arbitraryTxHash
  index <- arbitrary
  script <- B.pack <$> listOf arbitrary
  value <- arbitrary
  confirmations <- arbitrary
  txidx <- arbitrary
  xpub <- arbitraryMaybe $ arbitraryBinfoXPubPath net
  return BinfoUnspent {..}

arbitraryBinfoUnspents :: Network -> Gen BinfoUnspents
arbitraryBinfoUnspents net =
  BinfoUnspents <$> small (arbitraryBinfoUnspent net)

instance Arbitrary BinfoHeader where
  arbitrary =
    BinfoHeader
      <$> arbitraryBlockHash
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

arbitraryBinfoMempool :: Network -> Gen BinfoMempool
arbitraryBinfoMempool net =
  BinfoMempool <$> small (arbitraryBinfoTx net)

instance Arbitrary BinfoBlockInfos where
  arbitrary = BinfoBlockInfos <$> small arbitrary
