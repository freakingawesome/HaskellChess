module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import Hchess.Board.Test
import Hchess.Board.Spec

main :: IO ()
main = do
  defaultMain tests
  boardSpecs

tests :: TestTree
tests = testGroup "All Tests"
            [ boardSuite
            ]
