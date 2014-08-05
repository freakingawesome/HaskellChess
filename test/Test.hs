module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import Hchess.Board.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ boardSuite
            ]
