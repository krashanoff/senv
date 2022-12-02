module Main where

import Test.Tasty
import Test.Tasty.HUnit

import MyLib

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "example" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "statement" $
      parseStatement "KEY=VAL"
  ]