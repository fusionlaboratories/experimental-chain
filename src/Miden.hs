{-# LANGUAGE DataKinds #-}

module Miden where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.ByteString.Lazy qualified as L

import GHC.Generics

import Data.Field.Goldilocks

-- Miden Input File
-- https://github.com/0xPolygonMiden/examples/blob/main/examples/advice_provider.inputs
-- https://github.com/0xPolygonMiden/miden-vm/blob/4195475d75ab2d586bdb01d1ff3ea2cd626eaf7b/miden/src/cli/data.rs#L64
data InputFile = InputFile
    { operand_stack :: [String]
    , advice_stack :: Maybe [String]
    }
    deriving (Show, Generic)

instance Aeson.ToJSON InputFile

encodeInputFile :: InputFile -> Aeson.Encoding
encodeInputFile = Aeson.genericToEncoding Aeson.defaultOptions{Aeson.omitNothingFields = True}

writeInputFile :: FilePath -> InputFile -> IO ()
writeInputFile path = L.writeFile path . encodingToLazyByteString . encodeInputFile

-- >>> encodeInputFile inputFile
-- "{\"operand_stack\":[]}"
inputFile :: InputFile
inputFile = InputFile{operand_stack = [], advice_stack = Nothing}

-- >>> encodeInputFile (operandStack [1, 2, 3])
-- "{\"operand_stack\":[\"1\",\"2\",\"3\"]}"
operandStack :: [Field] -> InputFile
operandStack ops = inputFile{operand_stack = map show ops}

-- data MidenOut = MidenOut
--   { stack :: [String],
--     overflow_addrs :: [String]
--   }
--   deriving (Show, Generic)
