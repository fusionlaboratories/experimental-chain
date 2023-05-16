{-# LANGUAGE TemplateHaskell #-}

module Jusion.Wallet where

import Control.Lens.TH

type Address = Int
type Amount = Int
type Height = Int

data Wallet tx = Wallet
    { _address :: Address
    , _balance :: Amount
    , _transactions :: [tx]
    }
    deriving (Eq, Show)

makeLenses ''Wallet
