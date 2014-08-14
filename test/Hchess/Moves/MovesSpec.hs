module Hchess.Moves.MovesSpec where

import SpecHelper
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.List(sort)
import Data.Either.Unwrap

spec :: Spec
spec = do

  describe "A white pawn on a standard board at d2" $ do
    it "can move to either d3 or d4 if isolated" $ do
      moveTargets (stdPossibleMoves [(teamWhite,"pd2")] "d2") `shouldBe` ["d3", "d4"]
     
    it "can move to only d3 if d4 is occupied by a teammate" $ do
      moveTargets (stdPossibleMoves [(teamWhite,"pd2 pd4")] "d2") `shouldBe` ["d3"]

    it "can move to only d3 if d4 is occupied by an enemy" $ do
      moveTargets (stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd4")] "d2") `shouldBe` ["d3"]

    it "cannot move if d3 is occupied by a teammate" $ do
      stdPossibleMoves [(teamWhite,"pd2 pd3")] "d2" `shouldBe` Right []

    it "cannot move if d3 is occupied by an enemy" $ do
      stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pd3")] "d2" `shouldBe` Right []

    it "can move forward diagonally if occupied by enemies" $ do
      moveTargets (stdPossibleMoves [(teamWhite,"pd2"),(teamBlack,"pc3 pe3")] "d2") `shouldBe` ["c3", "d3", "d4", "e3"]
      
    it "cannot move forward diagonally if occupied by teammates" $ do
      moveTargets (stdPossibleMoves [(teamWhite,"pd2 pc3 pe3")] "d2") `shouldBe` ["d3", "d4"]
      
  describe "A white pawn on a standard board at d3" $ do
    it "can move to only d4 if isolated" $ do 
      moveTargets (stdPossibleMovesWithHistory [(teamWhite,"pd3")] [("d3",["d2"])] "d3") `shouldBe` ["d4"]
 
  describe "A black pawn on a standard board at d6" $ do
    it "can move to only d5 if isolated" $ do
      moveTargets (stdPossibleMovesWithHistory [(teamBlack,"pd6")] [("d6",["d7"])] "d6") `shouldBe` ["d5"]
  
  describe "Some internal tests of helper functions" $ do
    it "should be able to inject a piece's history" $ do
      injectPieceHistory (Piece teamBlack Pawn []) ["a1","b2"] `shouldBe` Piece teamBlack Pawn (locs ["a1","b2"])

  describe "A super tiny board" $ do
    it "should not allow pawns moving off the edge if below absolute north" $ do
      moveTargets (possibleMovesFromLocation (newBoard 2 2 [(teamWhite,"pa1")]) (loc "a1")) `shouldBe` ["a2"]
 
    it "should not allow pawns moving off the edge if at absolute north" $ do
      possibleMovesFromLocation (newBoard 2 2 [(teamWhite,"pa2")]) (loc "a2") `shouldBe` Right []
  
  describe "The board after a threatened pawn moves" $ do
    let b = b8x8 [(teamWhite,"pd2"),(teamBlack,"pe3")]

    describe "when a pawn moves without capturing" $ do
      let 
        m = move b (loc "d2",loc "d3")
        b' = getBoard m
      it "should have no captures" $ do
        captured b' `shouldBe` Map.empty

      it "should have no piece at d2" $ do
        pieceAt (loc "d2") b' `shouldBe` Right Nothing

    describe "when a pawn moves to capture" $ do
      let
        m = move b (loc "d2",loc "e3")
        b' = getBoard m

      it "make sure the original board has no captures" $ do
        Map.lookup teamWhite (getCaptures b) `shouldBe` Nothing

      it "should have one capture" $ do
        Map.lookup teamWhite (getCaptures b') `shouldBe` Just ([Piece teamBlack Pawn [loc "e3"]])

      it "should move the original piece to the target location" $ do
        fromRight (pieceAt (loc "e3") b') `shouldBe` Just (Piece teamWhite Pawn [loc "d2"])

      it "the new board should have the original board in its history" $ do
        getBoardHistory b' `shouldBe` [b]

  describe "Picking up the last piece on a board" $ do 
    let 
      b = b8x8 [(teamWhite,"pd2")]
      (b',p') = pickUpPiece b (loc "d2")

    it "should leave the board empty" $ do
      pieceCount b' `shouldBe` 0

    it "should give me back my pawn with its last recorded location" $ do
      p' `shouldBe` Just (Piece teamWhite Pawn [loc "d2"])

  describe "Picking up a piece from a new board" $ do 
    let 
      b = newStandardBoard teamWhite teamBlack
      (b',p') = pickUpPiece b (loc "e1")

    it "should leave the board with one fewer player" $ do
      pieceCount b' `shouldBe` 31

    it "just making sure the king existed on the original board" $ do
      pieceAt (loc "e1") b `shouldBe` Right (Just (Piece teamWhite King []))

    it "should leave the king spot empty" $ do
      pieceAt (loc "e1") b' `shouldBe` Right Nothing

    it "should give me back my king with its last recorded location" $ do
      p' `shouldBe` Just (Piece teamWhite King [loc "e1"])

  describe "Trying to pick up a piece from an empty square" $ do
    let
      b = newStandardBoard teamWhite teamBlack
    it "should give me the same board and no piece" $ do
      pickUpPiece b (loc "e3") `shouldBe` (b,Nothing)

  describe "Recording a capture" $ do
    let 
      b = b8x8 [(teamWhite,"pd2"),(teamBlack,"pe3 pc3")]
      b' = recordCapture b teamWhite (fromRight (pieceAt (loc "e3") b))

    it "should return the same board if no piece is captured" $ do
      recordCapture b teamWhite Nothing `shouldBe` b

    it "should add a team if it doesn't exist" $ do
      Map.toList (getCaptures b') `shouldBe` [(teamWhite,[Piece teamBlack Pawn []])]

    it "should append to an existing team's list of captured pieces" $ do
      let
        b'' = recordCapture b' teamWhite (fromRight (pieceAt (loc "c3") b'))
        blackPawn = Piece teamBlack Pawn []
      Map.toList (getCaptures b'') `shouldBe` [(teamWhite,[blackPawn,blackPawn])]

      --it "the second new board should have the original and first boards in its history" $ do
        --getBoardHistory b'' `shouldBe` [b,b']

  describe "En passant" $ do
    describe "on a board where black can perform en passant" $ do
      let
        b = newStandardBoard teamWhite teamBlack
        Move (_,_) b' = move b (loc "b7",loc "b4")
        Move (_,_) b'' = move b' (loc "a2",loc "a4")
        pm = possibleMovesFromLocation b'' (loc "b4") 

      it "should allow en passant from black at b4" $ do
        getTargetLocationsFromMoves pm `shouldContain` [loc "a3"]

    -- do the moves in reverse (white first) to make sure it can only be performed in the correct order

  where 
    teamBlack = Team South "Black"
    teamWhite = Team North "White"
    b8x8 ps = newBoard 8 8 ps
    stdPossibleMoves ts al = possibleMovesFromLocation (b8x8 ts) (fromAlgebraicLocation al)
    stdPossibleMovesWithHistory ts hist al = possibleMovesFromLocation (injectBoardHistory (b8x8 ts) hist) (fromAlgebraicLocation al)
    captured (Board _ capt _) = capt
    getBoard (Move _ b) = b
    getMap (Board m _ _) = m
    pieceCount b = Map.size (Map.filter (\x -> x /= Nothing) (getMap b)) 
    getCaptures (Board _ capt _) = capt
    getTargetLocationsFromMoves (Right []) = []
    getTargetLocationsFromMoves (Left _) = []
    getTargetLocationsFromMoves (Right (Move (_,to) _:ls)) = to : getTargetLocationsFromMoves (Right ls)
    getBoardHistory (Board _ _ hist) = hist

moveTargets :: Either String [Move] -> [String]
moveTargets (Right []) = []
moveTargets (Right (Move (_,to) _:ms)) = sort (toAlgebraicLocation to : moveTargets (Right ms))
moveTargets (Left _) = []

loc :: String -> Location
loc = fromAlgebraicLocation

locs :: [String] -> [Location]
locs [] = []
locs (x:xs) = loc x : locs xs

injectBoardHistory :: Board -> [(String,[String])] -> Board
injectBoardHistory b [] = b
injectBoardHistory (Board m capt bs) ((al,hist):hs) = 
  let updateOrFail p = if p == Nothing then error "Invalid location" else Just (Just (injectPieceHistory (fromJust p) hist))
  in injectBoardHistory (Board (Map.update updateOrFail (loc al) m) capt bs) hs 

injectPieceHistory :: Piece -> [String] -> Piece
injectPieceHistory (Piece t c _) hs = Piece t c (locs hs)


