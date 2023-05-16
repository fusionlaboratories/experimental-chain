{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Jusion where

import Data.Hashable (Hashable)
import Data.Map.Strict qualified as Map

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

-- TODO: Implement a simple model of Jusion

-- L1 Oracle, which contains
-- A Map of Blocks

-- Wallet
type Wallet = Wallet.Wallet Transaction

-- NOTE: Does not fully model the genesis
type Block = Block.Block Transaction

-- Transaction type
type Transaction = Transaction.Transaction

type Blockchain = Blockchain.Blockchain Block

type TransactionLog = TransactionLog.TransactionLog Transaction

type Network = Network.Network Block Transaction 
