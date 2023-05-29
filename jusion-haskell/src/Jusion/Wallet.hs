{-# LANGUAGE TemplateHaskell #-}
module Jusion.Wallet where

import Control.Lens.TH

import Jusion.Common

data Wallet tx = Wallet
    { _address :: Address
    , _balance :: Amount
    , _transactions :: [tx]
    }
    deriving (Eq, Show)

makeLenses ''Wallet
