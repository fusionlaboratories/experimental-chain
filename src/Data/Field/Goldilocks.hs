{-# LANGUAGE DataKinds, GeneralisedNewtypeDeriving, OverloadedLists, RecordWildCards #-}
module Data.Field.Goldilocks (Field(..), word8, word16, word32, word64, word128, word256) where

import Data.Aeson qualified as Aeson
import Data.FiniteField.PrimeField (PrimeField)

import Data.Field
import Data.Word
import Data.WideWord


-- The Order of Miden's Goldilocks field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
newtype Field = Field { unField :: PrimeField 18446744069414584321 }
    deriving (Eq, Ord, Bounded, Enum, Num)

instance Show Field where
    show = show . unField

-- Encoding Field Elements as JSON Strings
instance Aeson.ToJSON Field where
    toJSON = Aeson.toJSON . show . unField

-- Various operators
word8 :: Word8 -> ToField Field
word8 x = [ fromIntegral x ]

word16 :: Word16 -> ToField Field
word16 x = [ fromIntegral x ]

word32 :: Word32 -> ToField Field
word32 x = [ fromIntegral x ]

word64 :: Word64 -> ToField Field
word64 x = [ fromIntegral x ]

word128 :: Word128 -> ToField Field
word128 Word128{..} = [fromIntegral word128Hi64, fromIntegral word128Lo64]

word256 :: Word256 -> ToField Field
word256 Word256{..} = [fromIntegral word256hi, fromIntegral word256m1, fromIntegral word256m0, fromIntegral word256lo]