{-# LANGUAGE DataKinds #-}
module Jusion (F17, FMiden) where

import Data.FiniteField.PrimeField

-- A simple prime field
type F17 = PrimeField 17

-- The Order of Miden's underlying field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
type FMiden = PrimeField 18446744069414584321

-- going for a super simple implementation of the AST
data BinOp 
    -- Basic arithmetic
    = Add
    | Mul
    -- Equality check
    | Eq

data UnOp
    -- Unary operations
    = Inv
    | Neg

data Exp
    = Lit F17
    | BinOp BinOp Exp Exp
    | UnOp UnOp Exp

newtype Stmt
    = Assert Exp

newtype Program
    = Program [Stmt]
