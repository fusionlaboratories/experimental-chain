module Main where

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $ testCase "foo" $ 1 @?= 1