{-# LANGUAGE TemplateHaskell #-}
module Jusion.Transaction where

import Data.Hashable

import GHC.Generics

import Jusion.Common
import Control.Lens.TH

data Transaction = Transaction
    { _from :: Address
    , _to :: Address
    , _amount :: Amount
    }
    deriving (Eq, Show, Generic)

makeLenses ''Transaction

instance Hashable Transaction
