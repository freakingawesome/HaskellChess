module Hchess.Board.BoardSpec where

import SpecHelper
import TestUtil

spec :: Spec
spec = do

  describe "Board size" $ do
    it "should be 64 for a standard board" $ do
      boardSize (emptyBoard 8 8) `shouldBe` 64

    it "should not be constrained to a square" $ do
      boardSize (emptyBoard 7 3) `shouldBe` 21

  describe "Translating to algebraic notation" $ do 
    it "should locate the south-west corner" $ do
      toAlgebraicLocation (0,0) `shouldBe` "a1"
      
    it "should locate some square in the middle" $ do
      toAlgebraicLocation (2,6) `shouldBe` "c7"

  describe "Translating from algebraic notation" $ do
    it "should locate a1" $ do
      fromAlgebraicLocation "a1" `shouldBe` (0,0)

    it "should locate c7" $ do
      fromAlgebraicLocation "c7" `shouldBe` (2,6)

  describe "Character shorthand" $ do
    it "should identify a pawn" $ do
      fromAlgebraicCharacter 'p' `shouldBe` Pawn
  
    it "should identify a rook" $ do
      fromAlgebraicCharacter 'R' `shouldBe` Rook
  
    it "should identify a knight" $ do
      fromAlgebraicCharacter 'N' `shouldBe` Knight
  
    it "should identify a bishop" $ do
      fromAlgebraicCharacter 'B' `shouldBe` Bishop
  
    it "should identify a queen" $ do
      fromAlgebraicCharacter 'Q' `shouldBe` Queen
  
    it "should identify a king" $ do
      fromAlgebraicCharacter 'K' `shouldBe` King

  describe "Translating a character's location from shorthand" $ do
    it "should locate the pawn at b2" $ do
      fromAlgebraicCharacterLocation "pb2" `shouldBe` (Pawn,(1,1))
  
    it "should locate the king at d1" $ do
      fromAlgebraicCharacterLocation "Kd1" `shouldBe` (King,(3,0))
 
  describe "A new standard board" $ do
    it "should find a black rook at a8" $ do
      standardBoardPieceAt "a8" `shouldBe` Right (Just (Piece black Rook []))

    it "should find a white king at e1" $ do
      standardBoardPieceAt "e1" `shouldBe` Right (Just (Piece white King []))

    it "should find a black king at e8" $ do
      standardBoardPieceAt "e8" `shouldBe` Right (Just (Piece black King []))

    it "should find nothing in the middle of the board" $ do
      standardBoardPieceAt "d4" `shouldBe` Right Nothing

    it "should tell you if a location is invalid" $ do
      standardBoardPieceAt "z9" `shouldBe` Left "Invalid location" -- TODO: assert isLeft instead
 
