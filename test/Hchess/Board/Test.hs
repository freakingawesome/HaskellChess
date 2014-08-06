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
     testCase "newStandardBoardTest" newStandardBoardTest,
     testCase "newBoardTest_Pawn_a2" newBoardTest_Pawn_a2,
     testCase "nnewboardTest_Rook_a1_OppositeKing_d" newboardTest_Rook_a1_OppositeKing_d8,
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
teamBlack = Team South "Black"

teamWhite :: Team
teamWhite = Team North "White"

blackPawn :: Piece
blackPawn = Piece teamBlack Pawn []

newStandardBoardTest :: Assertion
newStandardBoardTest = do
  Just (Piece teamBlack Rook []) @=? pieceAt (0,7) b 
  Just (Piece teamWhite King []) @=? pieceAt (4,0) b
  Nothing @=? pieceAt (2,2) b
  where b = newStandardBoard teamWhite teamBlack

newBoardTest_Pawn_a2 :: Assertion
newBoardTest_Pawn_a2 = Just blackPawn @=? pieceAt (0,1) (newBoard 8 8 [(teamBlack,"pa2")])


newboardTest_Rook_a1_OppositeKing_d8 :: Assertion
newboardTest_Rook_a1_OppositeKing_d8 = do
  Just (Piece teamBlack Rook []) @=? pieceAt (0,0) b 
  Just (Piece teamWhite King []) @=? pieceAt (3,7) b
  Nothing @=? pieceAt (2,2) b
  where b = newBoard 8 8 [(teamBlack,"Ra1"),(teamWhite,"Kd8")]
