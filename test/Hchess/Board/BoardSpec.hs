module Hchess.Board.BoardSpec where

import SpecHelper
import qualified Data.Map as Map
import Data.Maybe (fromJust)

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
      standardBoardPieceAt "a8" `shouldBe` Just (Piece teamBlack Rook [])

    it "should find a white king at e1" $ do
      standardBoardPieceAt "e1" `shouldBe` Just (Piece teamWhite King [])

    it "should find a black king at e8" $ do
      standardBoardPieceAt "e8" `shouldBe` Just (Piece teamBlack King [])

    it "should find nothing in the middle of the board" $ do
      standardBoardPieceAt "d4" `shouldBe` Nothing

  describe "A white pawn on a standard board at d2" $ do
    it "can move to either d3 or d4 if isolated" $ do
      stdPossibleMoves [(teamWhite,"pd2")] "d2" `shouldBe` locs ["d3", "d4"]
     
    it "can move to only d3 if d4 is occupied by a teammate" $ do
      stdPossibleMoves [(teamWhite,"pd2 pd4")] "d2" `shouldBe` locs ["d3"]

    it "can move to only d3 if d4 is occupied by an enemy" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd4")] "d2" `shouldBe` locs ["d3"]

    it "cannot move if d3 is occupied by a teammate" $ do
      stdPossibleMoves [(teamWhite,"pd2 pd3")] "d2" `shouldBe` []

    it "cannot move if d3 is occupied by an enemy" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd3")] "d2" `shouldBe` []

  describe "A white pawn on a standard board at d3" $ do
    it "can move to only d4 if isolated" $ do 
      stdPossibleMovesWithHistory [(teamWhite,"pd3")] [("d3",["d2"])] "d3" `shouldBe` locs ["d4"]
 
  describe "A black pawn on a standard board at d6" $ do
    it "can move to only d5 if isolated" $ do
      stdPossibleMovesWithHistory [(teamBlack,"pd6")] [("d6",["d7"])] "d6" `shouldBe` locs ["d5"]
  
  describe "Some internal tests of helper functions" $ do
    it "should be able to inject a piece's history" $ do
      injectPieceHistory (Piece teamBlack Pawn []) ["a1","b2"] `shouldBe` Piece teamBlack Pawn (locs ["a1","b2"])
 
  where 
    boardSize (Board x _) = Map.size x
    teamBlack = Team South "Black"
    teamWhite = Team North "White"
    standardBoardPieceAt s = pieceAt (fromAlgebraicLocation s) (newStandardBoard teamWhite teamBlack)
    b8x8 ps = newBoard 8 8 ps
    stdPossibleMoves ts al = possibleMoves (b8x8 ts) (fromAlgebraicLocation al)
    stdPossibleMovesWithHistory ts hist al = possibleMoves (injectBoardHistory (b8x8 ts) hist) (fromAlgebraicLocation al)

loc :: String -> Location
loc = fromAlgebraicLocation

locs :: [String] -> [Location]
locs [] = []
locs (x:xs) = loc x : locs xs

injectBoardHistory :: Board -> [(String,[String])] -> Board
injectBoardHistory b [] = b
injectBoardHistory (Board m capt) ((al,hist):hs) = 
  let updateOrFail p = if p == Nothing then error "Invalid location" else Just (Just (injectPieceHistory (fromJust p) hist))
  in injectBoardHistory (Board (Map.update updateOrFail (loc al) m) capt) hs 

injectPieceHistory :: Piece -> [String] -> Piece
injectPieceHistory (Piece t c _) hs = Piece t c (locs hs)

