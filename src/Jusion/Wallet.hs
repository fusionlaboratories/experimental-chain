module Jusion.Wallet where

type Address = Int
type Amount = Int
type Height = Int

data Wallet tx = Wallet
    { _address :: Address
    , _balance :: Amount
    , _transactions :: [tx]
    }
    deriving (Eq, Show)