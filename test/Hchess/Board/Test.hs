module Hchess.Board.Test
    (boardSuite)
where
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Hchess.Board
import Data.Map (size)

boardSuite :: TestTree
boardSuite = testGroup "Board"
    [testCase "board size test" boardSizeTest,
     testCase "uneven board size test" unevenBoardSizeTest]

boardSizeTest :: Assertion
boardSizeTest = 64 @=? boardSize (emptyBoard 8 8)

unevenBoardSizeTest :: Assertion
unevenBoardSizeTest = 20 @=? boardSize (emptyBoard 4 5)

boardSize :: Board -> Int
boardSize (Board x) = size x

