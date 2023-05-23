{-# LANGUAGE DataKinds, GeneralisedNewtypeDeriving #-}
module Data.Field.Goldilocks (Field(..)) where

import Data.Aeson qualified as Aeson
import Data.Field
import Data.FiniteField.PrimeField (PrimeField)

-- The Order of Miden's Goldilocks field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
newtype Field = Field { unField :: PrimeField 18446744069414584321 }
    deriving (Eq, Ord, Bounded, Enum, Num)

instance Show Field where
    show = show . unField

instance Enum a => ToField a Field where
    toField = fromInteger . toInteger . fromEnum

-- Encoding Field Elements as JSON Strings
instance Aeson.ToJSON Field where
    toJSON = Aeson.toJSON . show . unField
