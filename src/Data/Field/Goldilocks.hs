{-# LANGUAGE DataKinds #-}
module Data.Field.Goldilocks (Field) where

import Data.FiniteField.PrimeField (PrimeField)

-- The Order of Miden's Goldilocks field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
type Field = PrimeField 18446744069414584321
