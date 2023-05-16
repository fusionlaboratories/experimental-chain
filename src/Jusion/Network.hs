{-# LANGUAGE TemplateHaskell #-}
module Jusion.Network where

import Control.Lens.TH 

import Jusion.TransactionLog
import Jusion.Blockchain
import Jusion.Ledger

data Network block tx = Network
    { _ledger :: Ledger
    , _blockchain :: Blockchain block
    , _transactionLog :: TransactionLog tx
    }
    deriving (Eq, Show)

makeLenses ''Network
