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
     testCase "toAlgebraicLocation" toAlgebraicLocationTest,
     testCase "toAlgebraicLocation2" toAlgebraicLocationTest2,
     testCase "fromAlgebraicLocation" fromAlgebraicLocationTest,
     testCase "fromAlgebraicLocation2" fromAlgebraicLocationTest2,
     testCase "fromAlgebraicCharacterTest" fromAlgebraicCharacterTest,
     --testCase "newStandardBoardTest" newStandardBoardTest,
     testCase "newBoardTest_Pawn_a2" newBoardTest_Pawn_a2,
     testCase "fromAlgebraicCharacterLocationTest" fromAlgebraicCharacterLocationTest,
     testCase "fromAlgebraicCharacterLocationTest2" fromAlgebraicCharacterLocationTest2]

boardSizeTest :: Assertion
boardSizeTest = 64 @=? boardSize (emptyBoard 8 8)

unevenBoardSizeTest :: Assertion
unevenBoardSizeTest = 20 @=? boardSize (emptyBoard 4 5)

boardSize :: Board -> Int
boardSize (Board x) = size x

toAlgebraicLocationTest :: Assertion
toAlgebraicLocationTest = "a1" @=? toAlgebraicLocation (0,0)
 
toAlgebraicLocationTest2 :: Assertion
toAlgebraicLocationTest2 = "c7" @=? toAlgebraicLocation (2,6)

fromAlgebraicLocationTest :: Assertion
fromAlgebraicLocationTest = (0,0) @=? fromAlgebraicLocation "a1" 
 
fromAlgebraicLocationTest2 :: Assertion
fromAlgebraicLocationTest2 = (2,6) @=? fromAlgebraicLocation "c7"

fromAlgebraicCharacterTest :: Assertion
fromAlgebraicCharacterTest = do
  Pawn @=? fromAlgebraicCharacter 'p'
  Rook @=? fromAlgebraicCharacter 'R'
  Knight @=? fromAlgebraicCharacter 'N'
  Bishop @=? fromAlgebraicCharacter 'B'
  Queen @=? fromAlgebraicCharacter 'Q'
  King @=? fromAlgebraicCharacter 'K'

fromAlgebraicCharacterLocationTest :: Assertion
fromAlgebraicCharacterLocationTest = (Pawn,(1,1)) @=? fromAlgebraicCharacterLocation "pb2"

fromAlgebraicCharacterLocationTest2 :: Assertion
fromAlgebraicCharacterLocationTest2 = (King,(3,0)) @=? fromAlgebraicCharacterLocation "Kd1"

teamBlack :: Team
teamBlack = Team North "Black"

blackPawn :: Piece
blackPawn = Piece teamBlack Pawn []

--newStandardBoardTest :: Assertion
--newStandardBoardTest = emptyBoard 8 8 @=? newStandardBoard []

newBoardTest_Pawn_a2 :: Assertion
newBoardTest_Pawn_a2 = Just blackPawn @=? pieceAt (0,1) (newBoard 8 8 [(teamBlack,"pa2")])


