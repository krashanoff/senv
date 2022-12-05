module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Senv

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [statementTests, fileTests]

statementTests = testGroup "statements"
    [
      testCase "trivial" $ do
        parseStatement "KEY=VAL"
        @?=
        Right (
        Assignment
            (Key "KEY")
            (Value "VAL")
        )
    , testCase "no value" $ do
        parseStatement "SOME_VAL="
        @?=
        Right (
            Assignment
            (Key "SOME_VAL")
            (Value "")
        )
    , testCase "optional export, no value, with comment" $ do
        parseStatement "export  SOME_VAL= # no value"
        @?=
        Right (
            Assignment
            (Key "SOME_VAL")
            (Value "")
        )
    , testCase "illegal ident" $
        assertBool "" (not
        (case parseStatement "1LLEGAL_IDENT=" of
            Left (e) -> False
            otherwise -> True
        ))
    ]

fileTests = testGroup "file"
    [
      testCase "trailing newline" $
        parseEnv "export SOME_VAL= # no comment\n"
        @?=
        Right [
            Assignment
            (Key "SOME_VAL")
            (Value "")
        ]
    , testCase "empty file" $
        parseEnv ""
        @?=
        Right []
    ]
