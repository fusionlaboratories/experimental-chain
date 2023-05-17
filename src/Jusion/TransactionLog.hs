{-# LANGUAGE TemplateHaskell #-}
module Jusion.TransactionLog where

import Data.Map.Strict qualified as Map

import Control.Lens.TH

import Jusion.Hash

newtype TransactionLog tx = TransactionLog
    { _byHash :: Map.Map (Hash tx) tx
    }
    deriving (Eq, Show)

makeLenses ''TransactionLog
