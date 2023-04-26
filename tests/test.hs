module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Jusion

main = defaultMain tests

tests :: TestTree
tests =
    testGroup "finite fields" [
        testCase "f17 wraps around" $ (1 :: F17) @?= (18 :: F17)
    ]