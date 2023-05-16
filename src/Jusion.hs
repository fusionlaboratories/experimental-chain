{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Jusion where

import Data.Hashable (Hashable)
import Data.Map.Strict qualified as Map

import GHC.Generics (Generic)

import Jusion.Hash
import Jusion.Common
import Jusion.Wallet qualified as Wallet
import Jusion.Block qualified as Block
import Jusion.Transaction qualified as Transaction

-- TODO: Implement a simple model of Jusion

-- L1 Oracle, which contains
-- A Map of Blocks


-- Wallet
type Wallet = Wallet.Wallet Transaction

-- NOTE: Does not fully model the genesis
type Block = Block.Block Transaction

-- Transaction type
type Transaction = Transaction.Transaction

newtype Ledger = Ledger
    { byAddress :: Map.Map Address Amount
    }
    deriving (Eq, Show)

data Blockchain = Blockchain
    { byHeight :: Map.Map Height Block
    , byHash :: Map.Map (Hash Block) Block
    }
    deriving (Eq, Show)

data TransactionLog = TransactionLog
    { byHeight :: Map.Map Height Transaction
    , byHash :: Map.Map (Hash Transaction) Transaction
    }
    deriving (Eq, Show)

data Network = Network
    { ledger :: Ledger
    , blockchain :: Blockchain
    , transactionLog :: TransactionLog
    }
    deriving (Eq, Show)
