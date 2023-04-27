{-# LANGUAGE DataKinds #-}
module Jusion (F17) where

import Data.FiniteField.PrimeField

-- A simple prime field
type F17 = PrimeField 17


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
