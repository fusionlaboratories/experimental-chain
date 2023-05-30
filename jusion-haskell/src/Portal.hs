{-# OPTIONS_GHC -Wno-orphans #-}

module Portal where

import Data.Default
import Data.WideWord
import Data.Word

import GHC.Generics

data Transcript = Transcript
  { -- choose which L1 we are reffering to
    l1 :: Word32
  , -- get the block
    blockNumber :: Word64
  , blockHash :: Word256
  , -- get the transaction
    transactionIndex :: Word32
  , transactionHash :: Word256
  , -- perform the transaction on L2
    destAddress :: Word256
  , amount :: Word32
  }
  deriving (Generic, Show)

instance Default Transcript
instance Default Word256