{-# LANGUAGE DataKinds #-}

module Data.Field.F17 (Field(..)) where

import Data.FiniteField.PrimeField (PrimeField)

-- A simple prime field
newtype Field = Field { unField :: PrimeField 17 }
    deriving (Eq, Ord, Bounded, Enum, Num, Show)
