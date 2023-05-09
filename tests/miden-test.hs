module Main where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Program

import Miden (InputFile (..), inputFile, writeInputFile)
import System.Exit
import System.Process

import Control.Monad

-- Aeson Encoding
import Data.Aeson

i :: InputFile
i = inputFile{operand_stack = ["0", "18446744069414584321"]}

main :: IO ()
main = defaultMain tests

testCase :: TestName -> TestTree
testCase name =
    testGroup
        name
        [ goldenVsFile "input" inputGolden inputFile (writeInputFile inputFile i)
        , after AllSucceed "input" (testProgram "compilation" "miden" ["run", "-a", masmFile, "-i", inputFile, "-o", outputFile] Nothing)
        , after AllSucceed "compilation" (goldenVsFile "output" outputGolden outputFile (pure ()))
        ]
  where
    inputFile = name ++ ".input"
    inputGolden = inputFile ++ ".golden"
    masmFile = name ++ ".masm"
    outputFile = name ++ ".output"
    outputGolden = outputFile ++ ".golden"

tests :: TestTree
tests =
    testGroup
        "miden tests"
        [ testCase "tests/test"
        ]

{-

Quick sketch of the compilation pipeline:

- generate inputs
- generate assembly file

-}