module Jusion.Ledger where

import Jusion.Common

import Data.Map.Strict as Map

newtype Ledger = Ledger
    { _byAddress :: Map.Map Address Amount
    }
    deriving (Eq, Show)