{-# LANGUAGE DataKinds #-}

module Data.Field.F17 (Field) where

import Data.FiniteField.PrimeField (PrimeField)

-- A simple prime field
type Field = PrimeField 17
