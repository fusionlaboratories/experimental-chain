module Main where

import Control.Monad (unless)

import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (dropExtension, makeRelative, (</>))

import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsFile)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Patterns.Types qualified as E
import Test.Tasty.Program (testProgram)

import Data.Field.F17 qualified as F17
import Miden (InputFile (..), inputFile, writeInputFile)
import Test.Tasty.Patterns.Types (Expr (StringLit))

-- midenInput :: InputFile
-- midenInput = inputFile{operand_stack = ["0", "18446744069414584321"]}

main :: IO ()
main = do
    -- TODO Refactor this to be loaded in tests...
    putStrLn "Searching for test cases"
    cwd <- getCurrentDirectory
    let testDir = cwd </> "tests"
    exists <- doesDirectoryExist testDir
    unless exists $
        error "not found the directory with tests"
    putStrLn $ "Current directory: " ++ cwd
    midenFiles <- fmap (dropExtension . makeRelative cwd) <$> findByExtension [".masm"] testDir
    let testCount = length midenFiles
    if testCount >= 0
        then putStrLn $ "Found " ++ show (length midenFiles) ++ " test files"
        else error $ "Did not find any test cases in " ++ testDir
    defaultMain $
        testGroup
            "jusion"
            [ tests
            , testGroup "miden" (fmap midenTestCase midenFiles)
            ]

tests :: TestTree
tests =
    testGroup
        "finite fields"
        [ testCase "f17 wraps around" $ (0 :: F17.Field) @?= (17 :: F17.Field)
        ]

midenTestCase :: TestName -> TestTree
midenTestCase name =
    testGroup
        name
        [ -- goldenVsFile "input" inputGolden inputFile (writeInputFile inputFile midenInput)
          -- , after AllSucceed "input" (testProgram "compilation" "miden" ["run", "-a", masmFile, "-i", inputFile, "-o", outputFile] Nothing)
          testProgram "compilation" "miden" ["run", "-a", masmFile, "-i", inputFile, "-o", outputFile] Nothing
        , after_ AllSucceed compilation (goldenVsFile "output" outputGolden outputFile (pure ()))
        ]
  where
    inputFile = name ++ ".input"
    inputGolden = inputFile ++ ".golden"
    masmFile = name ++ ".masm"
    outputFile = name ++ ".output"
    outputGolden = outputFile ++ ".golden"
    -- Match only the the compilation step in your own group
    -- \$(NF - 1) == name && $(NF) == "compilation"
    compilation = E.And (E.EQ (E.Field (E.Sub E.NF (E.IntLit 1))) (E.StringLit name)) (E.EQ (E.Field E.NF) (E.StringLit "compilation"))