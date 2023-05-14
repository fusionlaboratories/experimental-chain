{-# LANGUAGE DataKinds #-}

module Field (F17) where

import Data.FiniteField.PrimeField (PrimeField)

-- A simple prime field
type F17 = PrimeField 17