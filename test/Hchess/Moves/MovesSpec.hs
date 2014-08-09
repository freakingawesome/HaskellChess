module Hchess.Moves.MovesSpec where

import SpecHelper
import qualified Data.Map as Map
import Data.Maybe(fromJust)

spec :: Spec
spec = do

  describe "A white pawn on a standard board at d2" $ do
    it "can move to either d3 or d4 if isolated" $ do
      stdPossibleMoves [(teamWhite,"pd2")] "d2" `shouldBe` Right (locs ["d3", "d4"])
     
    it "can move to only d3 if d4 is occupied by a teammate" $ do
      stdPossibleMoves [(teamWhite,"pd2 pd4")] "d2" `shouldBe` Right (locs ["d3"])

    it "can move to only d3 if d4 is occupied by an enemy" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd4")] "d2" `shouldBe` Right (locs ["d3"])

    it "cannot move if d3 is occupied by a teammate" $ do
      stdPossibleMoves [(teamWhite,"pd2 pd3")] "d2" `shouldBe` Right []

    it "cannot move if d3 is occupied by an enemy" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd3")] "d2" `shouldBe` Right []

    it "can move forward diagonally if occupied by enemies" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pc3 pe3")] "d2" `shouldBe` Right (locs ["c3", "d3", "d4", "e3"])
      
    it "cannot move forward diagonally if occupied by teammates" $ do
      stdPossibleMoves [(teamWhite,"pd2 pc3 pe3")] "d2" `shouldBe` Right (locs ["d3", "d4"])
      
  describe "A white pawn on a standard board at d3" $ do
    it "can move to only d4 if isolated" $ do 
      stdPossibleMovesWithHistory [(teamWhite,"pd3")] [("d3",["d2"])] "d3" `shouldBe` Right (locs ["d4"])
 
  describe "A black pawn on a standard board at d6" $ do
    it "can move to only d5 if isolated" $ do
      stdPossibleMovesWithHistory [(teamBlack,"pd6")] [("d6",["d7"])] "d6" `shouldBe` Right (locs ["d5"])
  
  describe "Some internal tests of helper functions" $ do
    it "should be able to inject a piece's history" $ do
      injectPieceHistory (Piece teamBlack Pawn []) ["a1","b2"] `shouldBe` Piece teamBlack Pawn (locs ["a1","b2"])

  describe "A super tiny board" $ do
    it "should not allow pawns moving off the edge if below absolute north" $ do
      possibleMovesFromLocation (newBoard 2 2 [(teamWhite,"pa1")]) (loc "a1") `shouldBe` Right (locs ["a2"])
 
    it "should not allow pawns moving off the edge if at absolute north" $ do
      possibleMovesFromLocation (newBoard 2 2 [(teamWhite,"pa2")]) (loc "a2") `shouldBe` Right []
 
  where 
    teamBlack = Team South "Black"
    teamWhite = Team North "White"
    b8x8 ps = newBoard 8 8 ps
    stdPossibleMoves ts al = possibleMovesFromLocation (b8x8 ts) (fromAlgebraicLocation al)
    stdPossibleMovesWithHistory ts hist al = possibleMovesFromLocation (injectBoardHistory (b8x8 ts) hist) (fromAlgebraicLocation al)

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


