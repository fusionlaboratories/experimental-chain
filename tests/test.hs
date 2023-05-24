{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (unless)

import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (dropExtension, makeRelative, (</>))

import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsFile)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Patterns.Types qualified as E
import Test.Tasty.Program (testProgram)

import Data.Default
import Data.Field
import Data.Field.F17 qualified as F17
import Data.Field.Goldilocks as Goldilocks
import Data.Word
import Data.WideWord.Word256

import GHC.Exts
import GHC.Generics

import Miden (InputFile (..), writeInputFile)
-- import Test.Tasty.Patterns.Types (Expr (StringLit))

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
            , -- , testGroup "miden" (fmap midenTestCase midenFiles)
              testOracle
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
    -- inputGolden = inputFile ++ ".golden"
    masmFile = name ++ ".masm"
    outputFile = name ++ ".output"
    outputGolden = outputFile ++ ".golden"
    -- Match only the the compilation step in your own group
    -- \$(NF - 1) == name && $(NF) == "compilation"
    compilation = E.And (E.EQ (E.Field (E.Sub E.NF (E.IntLit 1))) (E.StringLit name)) (E.EQ (E.Field E.NF) (E.StringLit "compilation"))

-- example oracle transcript
data Transcript = Transcript
    { l1 :: Word32
    , blockHeight :: Word32
    , blockHash :: Word256
    , txSlot :: Word32
    , txHash :: Word256
    , destAddress :: Word256
    , amount :: Word32
    }
    deriving (Generic, Show)

instance Default Word256
instance Default Transcript


-- >>> blockHash sampleTranscript
-- 73585536169652625379848022803085830090075642614829910185584428049837339717859
sampleTranscript :: Transcript
sampleTranscript = Transcript
    0
    17285321
    0xa2aff0019efe98937608c930ca294e8db2ab2ebffb50eb9b5a801e69f88c30e3
    0
    0xa2aff0019efe98937608c930ca294e8db2ab2ebffb50eb9b5a801e69f88c30e3
    0x123456
    20


transcript :: Transcript -> ToField Goldilocks.Field
transcript Transcript{..} = do
    Goldilocks.word32 l1
    Goldilocks.word32 blockHeight
    Goldilocks.word256 blockHash
    Goldilocks.word32 txSlot
    Goldilocks.word256 txHash
    Goldilocks.word256 destAddress
    Goldilocks.word32 amount

-- TODO: Encode those inputs as Miden Input File
-- >>> publicInputs
-- [0,17285321,11722852244821809299,8505269107676499597,12874435361858710427,6521245700922814691,0,11722852244821809299,8505269107676499597,12874435361858710427,6521245700922814691,0,0,0,1193046,20]
publicInputs :: [Goldilocks.Field]
publicInputs =  toList $ transcript sampleTranscript

afterExpr :: TestName -> TestName -> E.Expr
afterExpr group test = E.And (E.EQ (E.Field (E.Sub E.NF (E.IntLit 1))) (E.StringLit group)) (E.EQ (E.Field E.NF) (E.StringLit test))

testOracle :: TestTree
testOracle =
    testGroup
        groupName
        [
            -- generate input
            inputStep
            -- run compilation
            , after_ AllSucceed inputAftr runStep
            -- check output
            , after_ AllSucceed runAftr outputStep
            -- verify
            , after_ AllSucceed outputAftr verifyStep
        ]
        where
            groupName = "tests" </> "test_oracle"

            -- Input Step
            inputAftr = afterExpr groupName "input"
            inputStep = goldenVsFile "input" goldenFile actualFile $
                writeInputFile actualFile def
                where
                    goldenFile = actualFile ++ ".golden"
                    actualFile = groupName ++ ".input"

            -- Compilation Step
            runAftr = afterExpr groupName "run"
            runStep = testProgram "run" "miden" ["run", "-a", masmFile, "-i", inputFile, "-o", outputFile] Nothing
                where
                    masmFile = groupName ++ ".masm"
                    inputFile = groupName ++ ".input"
                    outputFile = groupName ++ ".output"

            -- Output Step
            outputAftr = afterExpr groupName "output"
            outputStep = goldenVsFile "output" goldenFile actualFile (pure ())
                where
                    goldenFile = actualFile ++ ".golden"
                    actualFile = groupName ++ ".output"

            -- Verify Step
            verifyStep = testCase "verify" (pure ())
