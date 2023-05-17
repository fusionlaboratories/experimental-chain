module Nightfall where

import Data.Function
import Data.Field.Goldilocks

data Exp where
    Lit :: Field -> Exp
    BinOp :: BinOp -> Exp -> Exp -> Exp
    UnOp :: UnOp -> Exp -> Exp

data BinOp = Add | Sub | Mul

delta2 :: BinOp -> Field -> Field -> Field
delta2 Add = (+)
delta2 Sub = (-)
delta2 Mul = (*)

data UnOp = Neg | Abs | SigNum

delta1 :: UnOp -> Field -> Field
delta1 Neg = negate
delta1 Abs = id
delta1 SigNum = const 1

instance Num Exp where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UnOp Neg
  abs = UnOp Abs
  signum = UnOp SigNum
  fromInteger = Lit . fromInteger

eval :: Exp -> Field
eval (Lit n) = n
eval (BinOp binOp a b) = (delta2 binOp `on` eval) a b
eval (UnOp unOp a) = delta1 unOp $ eval a
