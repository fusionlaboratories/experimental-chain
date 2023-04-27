{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Miden where

import Data.Aeson qualified as Aeson
import GHC.Generics

import Data.FiniteField qualified as PF
import Data.FiniteField.PrimeField qualified as PF

-- The Order of Miden's underlying field is
-- >>> 2 ^ 64 - 2 ^ 32 + 1
-- 18446744069414584321
type Field = PF.PrimeField 18446744069414584321

instance Aeson.ToJSON Field where
    toJSON = Aeson.toJSON . show

-- Miden Input File
-- https://github.com/0xPolygonMiden/examples/blob/main/examples/advice_provider.inputs
-- https://github.com/0xPolygonMiden/miden-vm/blob/4195475d75ab2d586bdb01d1ff3ea2cd626eaf7b/miden/src/cli/data.rs#L64
data InputFile = InputFile
    { operand_stack :: [Field]
    , advice_stack :: Maybe [String]
    }
    deriving (Show, Generic)

instance Aeson.ToJSON InputFile

encodeInputFile :: InputFile -> Aeson.Encoding
encodeInputFile = Aeson.genericToEncoding Aeson.defaultOptions{Aeson.omitNothingFields = True}

-- >>> encodeInputFile inputFile
-- "{\"operand_stack\":[]}"
inputFile :: InputFile
inputFile = InputFile{operand_stack = [], advice_stack = Nothing}

-- >>> encodeInputFile (operandStack [1, 2, 3])
-- "{\"operand_stack\":[\"1\",\"2\",\"3\"]}"
operandStack :: [Field] -> InputFile
operandStack ops = inputFile{operand_stack = ops}

-- data MidenOut = MidenOut
--   { stack :: [String],
--     overflow_addrs :: [String]
--   }
--   deriving (Show, Generic)
