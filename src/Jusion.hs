{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Jusion where

import Data.Hashable (Hashable)
import Data.Map.Strict qualified as Map
import Data.FiniteField.PrimeField qualified as PF

import GHC.Generics (Generic)

import Jusion.Block qualified as Block
import Jusion.Common
import Jusion.Hash
import Jusion.Transaction qualified as Transaction
import Jusion.Wallet qualified as Wallet
import Jusion.Ledger
import Jusion.Blockchain qualified as Blockchain
import Jusion.TransactionLog qualified as TransactionLog
import Jusion.Network qualified as Network

import Data.Field.Goldilocks

-- TODO: Implement a simple model of Jusion
type Wallet = Wallet.Wallet Transaction
type Block = Block.Block Transaction
type Transaction = Transaction.Transaction
type Blockchain = Blockchain.Blockchain Block
type TransactionLog = TransactionLog.TransactionLog Transaction
type Network = Network.Network Block Transaction 
