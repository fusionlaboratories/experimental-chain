module Portal where

import Data.Word
import Data.WideWord

import GHC.Generics

data Transcript = Transcript
    { l1 :: Word32
    , blockHeight :: Word32
    , blockHash :: Word256
    , txSlot :: Word32
    , txHash :: Word256
    , destAddress :: Word256
    , amount :: Word32
    }
    deriving (Generic, Show)