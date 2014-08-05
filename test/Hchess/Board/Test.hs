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
     testCase "uneven board size test" unevenBoardSizeTest,
     testCase "toAlgebraic" toAlgebraicTest,
     testCase "toAlgebraic2" toAlgebraicTest2,
     testCase "fromAlgebraic" fromAlgebraicTest,
     testCase "fromAlgebraic2" fromAlgebraicTest2]

boardSizeTest :: Assertion
boardSizeTest = 64 @=? boardSize (emptyBoard 8 8)

unevenBoardSizeTest :: Assertion
unevenBoardSizeTest = 20 @=? boardSize (emptyBoard 4 5)

boardSize :: Board -> Int
boardSize (Board x) = size x

toAlgebraicTest :: Assertion
toAlgebraicTest = "a1" @=? toAlgebraic (0,0)
 
toAlgebraicTest2 :: Assertion
toAlgebraicTest2 = "c7" @=? toAlgebraic (2,6)

fromAlgebraicTest :: Assertion
fromAlgebraicTest = (0,0) @=? fromAlgebraic "a1" 
 
fromAlgebraicTest2 :: Assertion
fromAlgebraicTest2 = (2,6) @=? fromAlgebraic "c7"
