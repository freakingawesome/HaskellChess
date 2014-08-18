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
      moveTargets (stdPossibleMoves [(white,"pd2")] "d2") `shouldBe` ["d3", "d4"]
     
    it "can move to only d3 if d4 is occupied by a teammate" $ do
      moveTargets (stdPossibleMoves [(white,"pd2 pd4")] "d2") `shouldBe` ["d3"]

    it "can move to only d3 if d4 is occupied by an enemy" $ do
      moveTargets (stdPossibleMoves [(white,"pd2"),(black,"pd4")] "d2") `shouldBe` ["d3"]

    it "cannot move if d3 is occupied by a teammate" $ do
      stdPossibleMoves [(white,"pd2 pd3")] "d2" `shouldBe` Right []

    it "cannot move if d3 is occupied by an enemy" $ do
      stdPossibleMoves [(white,"pd2"),(black,"pd3")] "d2" `shouldBe` Right []

    it "can move forward diagonally if occupied by enemies" $ do
      moveTargets (stdPossibleMoves [(white,"pd2"),(black,"pc3 pe3")] "d2") `shouldBe` ["c3", "d3", "d4", "e3"]
      
    it "cannot move forward diagonally if occupied by teammates" $ do
      moveTargets (stdPossibleMoves [(white,"pd2 pc3 pe3")] "d2") `shouldBe` ["d3", "d4"]
      
  describe "A white pawn on a standard board at d3" $ do
    it "can move to only d4 if isolated" $ do 
      moveTargets (stdPossibleMovesWithHistory [(white,"pd3")] [("d3",["d2"])] "d3") `shouldBe` ["d4"]
 
  describe "A black pawn on a standard board at d6" $ do
    it "can move to only d5 if isolated" $ do
      moveTargets (stdPossibleMovesWithHistory [(black,"pd6")] [("d6",["d7"])] "d6") `shouldBe` ["d5"]
  
  describe "Some internal tests of helper functions" $ do
    it "should be able to inject a piece's history" $ do
      injectPieceHistory (Piece black Pawn []) ["a1","b2"] `shouldBe` Piece black Pawn (locs ["a1","b2"])

  describe "A super tiny board" $ do
    it "should not allow pawns moving off the edge if below absolute north" $ do
      moveTargets (possibleMovesFromLocation (newBoard 2 2 [(white,"pa1")]) (loc "a1")) `shouldBe` ["a2"]
 
    it "should not allow pawns moving off the edge if at absolute north" $ do
      possibleMovesFromLocation (newBoard 2 2 [(white,"pa2")]) (loc "a2") `shouldBe` Right []
  
  describe "The board after a threatened pawn moves" $ do
    let b = b8x8 [(white,"pd2"),(black,"pe3")]

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
        Map.lookup white (getCaptures b) `shouldBe` Nothing

      it "should have one capture" $ do
        Map.lookup white (getCaptures b') `shouldBe` Just ([Piece black Pawn [loc "e3"]])

      it "should move the original piece to the target location" $ do
        fromRight (pieceAt (loc "e3") b') `shouldBe` Just (Piece white Pawn [loc "d2"])

      it "the new board should have the original board in its history" $ do
        getBoardHistory b' `shouldBe` [b]

  describe "Picking up the last piece on a board" $ do 
    let 
      b = b8x8 [(white,"pd2")]
      (b',p') = pickUpPiece b (loc "d2")

    it "should leave the board empty" $ do
      pieceCount b' `shouldBe` 0

    it "should give me back my pawn with its last recorded location" $ do
      p' `shouldBe` Just (Piece white Pawn [loc "d2"])

  describe "Picking up a piece from a new board" $ do 
    let 
      b = newStandardBoard white black
      (b',p') = pickUpPiece b (loc "e1")

    it "should leave the board with one fewer player" $ do
      pieceCount b' `shouldBe` 31

    it "just making sure the king existed on the original board" $ do
      pieceAt (loc "e1") b `shouldBe` Right (Just (Piece white King []))

    it "should leave the king spot empty" $ do
      pieceAt (loc "e1") b' `shouldBe` Right Nothing

    it "should give me back my king with its last recorded location" $ do
      p' `shouldBe` Just (Piece white King [loc "e1"])

  describe "Trying to pick up a piece from an empty square" $ do
    let
      b = newStandardBoard white black
    it "should give me the same board and no piece" $ do
      pickUpPiece b (loc "e3") `shouldBe` (b,Nothing)

  describe "Recording a capture" $ do
    let 
      b = b8x8 [(white,"pd2"),(black,"pe3 pc3")]
      b' = recordCapture b white (fromRight (pieceAt (loc "e3") b))

    it "should return the same board if no piece is captured" $ do
      recordCapture b white Nothing `shouldBe` b

    it "should add a team if it doesn't exist" $ do
      Map.toList (getCaptures b') `shouldBe` [(white,[Piece black Pawn []])]

    it "should append to an existing team's list of captured pieces" $ do
      let
        b'' = recordCapture b' white (fromRight (pieceAt (loc "c3") b'))
        blackPawn = Piece black Pawn []
      Map.toList (getCaptures b'') `shouldBe` [(white,[blackPawn,blackPawn])]

  describe "En passant" $ do
    describe "on a board where black can perform en passant on the right" $ do
      let
        b = newStandardBoard white black
        Move (_,_) b' = move b (loc "b7",loc "b4") -- not a legal move, I'm cheating for now
        Move (_,_) b'' = move b' (loc "a2",loc "a4")
        pm = possibleMovesFromLocation b'' (loc "b4") 

      it "should allow en passant from black at b4" $ do
        getTargetLocationsFromMoves pm `shouldContain` [loc "a3"]

      it "the second new board should have the original and first boards in its history" $ do
        getBoardHistory b'' `shouldBe` [b,b']

      describe "the resulting board" $ do
        let
          possibleBoard = getBoardFromPossibleMoves pm (loc "b4",loc "a3")
      
        it "should be missing the captured piece" $ do
          pieceAt (loc "a4") possibleBoard `shouldBe` Right Nothing

        it "should have the missing pawn in the captured list" $ do
          captured possibleBoard `shouldBe` Map.fromList [(black,[Piece white Pawn [loc "a2",loc "a4"]])]

    describe "on a board where black can perform en passant on the left" $ do
      let
        b = newStandardBoard white black
        Move (_,_) b' = move b (loc "b7",loc "b4") -- not a legal move, I'm cheating for now
        Move (_,_) b'' = move b' (loc "c2",loc "c4")
        pm = possibleMovesFromLocation b'' (loc "b4") 

      it "should allow en passant from black at b4" $ do
        getTargetLocationsFromMoves pm `shouldContain` [loc "c3"]

      it "the second new board should have the original and first boards in its history" $ do
        getBoardHistory b'' `shouldBe` [b,b']

      describe "the resulting board" $ do
        let
          possibleBoard = getBoardFromPossibleMoves pm (loc "b4",loc "c3")
      
        it "should be missing the captured piece" $ do
          pieceAt (loc "c4") possibleBoard `shouldBe` Right Nothing

        it "should have the missing pawn in the captured list" $ do
          captured possibleBoard `shouldBe` Map.fromList [(black,[Piece white Pawn [loc "c2",loc "c4"]])]

    describe "when the positions are right for en passant but the opportunity has passed" $ do
      let
        b = newStandardBoard white black
        Move (_,_) b' = move b (loc "a2",loc "a3")
        Move (_,_) b'' = move b' (loc "b7",loc "b4") -- not a legal move, I'm cheating for now
        Move (_,_) b''' = move b'' (loc "a3",loc "a4")
        pm = possibleMovesFromLocation b''' (loc "b4") 

      it "should not allow en passant from black at b4" $ do
        loc "a3" `elem` getTargetLocationsFromMoves pm `shouldBe` False

  describe "A rook" $ do

    it "can move in straight lines" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4")] "d4")) `shouldBe` sort [
        "d5","d6","d7","d8",
        "e4","f4","g4","h4",
        "d3","d2","d1",
        "c4","b4","a4"]

    it "can move in straight lines up to and including capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4"),(black,"pd7 pg4 pd2 pc4")] "d4")) `shouldBe` sort [
        "d5","d6","d7",
        "e4","f4","g4",
        "d3","d2",
        "c4"]

    it "cannot budge teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4 pd7 pg4 pd2 pc4")] "d4")) `shouldBe` sort [
        "d5","d6",
        "e4","f4",
        "d3"]

  describe "A knight" $ do

    it "can do what a knight does" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4")] "d4")) `shouldBe` sort [
        "e6","f5","f3","e2","c2","b3","b5","c6"]

    it "can capture enemies at any of those squares" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4"),(black,"pe6 pf3 pc2 pb5")] "d4")) `shouldBe` sort [
        "e6","f5","f3","e2","c2","b3","b5","c6"]

    it "cannot budge teammates" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4 pe6 pf3 pc2 pb5")] "d4")) `shouldBe` sort [
        "f5","e2","b3","c6"]

  describe "A bishop" $ do

    it "can move in diagonal lines" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4")] "d4")) `shouldBe` sort [
        "e5","f6","g7","h8",
        "e3","f2","g1",
        "c3","b2","a1",
        "c5","b6","a7"]

    it "can move in diagonal lines up to and including capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4"),(black,"pg7 pf2 pc3 pb6")] "d4")) `shouldBe` sort [
        "e5","f6","g7",
        "e3","f2",
        "c3",
        "c5","b6"]

    it "cannot budge teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4 pg7 pf2 pc3 pb6")] "d4")) `shouldBe` sort [
        "e5","f6",
        "e3",
        "c5"]

  describe "A queen" $ do

    it "can do whatever she damn well pleases" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Qd4")] "d4")) `shouldBe` sort [
        -- horizontal (copied from rook)
        "d5","d6","d7","d8",
        "e4","f4","g4","h4",
        "d3","d2","d1",
        "c4","b4","a4",
        -- diagonal (copied from bishop)
        "e5","f6","g7","h8",
        "e3","f2","g1",
        "c3","b2","a1",
        "c5","b6","a7"]

    it "can do whatever she damn well pleases up to and including capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Qd4"),(black,"pd7 pg4 pd2 pc4 pg7 pf2 pc3 pb6")] "d4")) `shouldBe` sort [
        -- horizontal (copied from rook)
        "d5","d6","d7",
        "e4","f4","g4",
        "d3","d2",
        "c4",
        -- diagonal (copied from bishop)
        "e5","f6","g7",
        "e3","f2",
        "c3",
        "c5","b6"]

    it "can do whatever she damn well pleases except for budging her teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Qd4 pd7 pg4 pd2 pc4 pg7 pf2 pc3 pb6")] "d4")) `shouldBe` sort [
        -- horizontal (copied from rook)
        "d5","d6",
        "e4","f4",
        "d3",
        -- diagonal (copied from bishop)
        "e5","f6",
        "e3",
        "c5"]

  where 
    black = Team South "Black"
    white = Team North "White"
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
    getBoardFromPossibleMoves pm ls = getBoard (head (filter (\(Move m _) -> m == ls) (fromRight pm)))

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


