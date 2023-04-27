module Main where

import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?=) )

import Jusion

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "finite fields" [
        testCase "f17 wraps around" $ (0 :: F17) @?= (17 :: F17)
    ]