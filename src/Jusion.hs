module Jusion where




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
