{-# LANGUAGE DataKinds #-}
module Data.Field.Goldilocks (Field(..)) where

import Data.Field
import Data.FiniteField.PrimeField (PrimeField)

-- The Order of Miden's Goldilocks field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
newtype Field = Field { unField :: PrimeField 18446744069414584321 }
    deriving (Eq, Ord, Bounded, Enum, Num, Show)

instance Enum a => ToField a Field where
    toField = fromInteger . toInteger . fromEnum