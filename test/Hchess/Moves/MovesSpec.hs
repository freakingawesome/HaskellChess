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
      moveTargets (stdPossibleMoves [(white,"pd2 Kh1"),(black,"pc3 pe3 Kh8")] "d2") `shouldBe` ["c3", "d3", "d4", "e3"]
      
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
      moveTargets (possibleMovesFromLocation (newBoard 2 2 [(white,"pa1")]) (loc "a1") 1) `shouldBe` replicate 4 "a2" -- 4 because of pawn promotion
 
    it "should not allow pawns moving off the edge if at absolute north" $ do
      possibleMovesFromLocation (newBoard 2 2 [(white,"pa2")]) (loc "a2") 1 `shouldBe` Right []
  
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
        pm = possibleMovesFromLocation b'' (loc "b4") 1

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
        pm = possibleMovesFromLocation b'' (loc "b4") 1

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
        pm = possibleMovesFromLocation b''' (loc "b4") 1

      it "should not allow en passant from black at b4" $ do
        loc "a3" `elem` getTargetLocationsFromMoves pm `shouldBe` False

  describe "A rook" $ do

    it "can move in straight lines" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4 Kh1")] "d4")) `shouldBe` sort [
        "d5","d6","d7","d8",
        "e4","f4","g4","h4",
        "d3","d2","d1",
        "c4","b4","a4"]

    it "can move in straight lines up to and including capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4 Kh1"),(black,"pd7 pg4 pd2 pc4 Kh8")] "d4")) `shouldBe` sort [
        "d5","d6","d7",
        "e4","f4","g4",
        "d3","d2",
        "c4"]

    it "cannot budge teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Rd4 pd7 pg4 pd2 pc4 Kh1")] "d4")) `shouldBe` sort [
        "d5","d6",
        "e4","f4",
        "d3"]

  describe "A knight" $ do

    it "can do what a knight does" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4 Kh1")] "d4")) `shouldBe` sort [
        "e6","f5","f3","e2","c2","b3","b5","c6"]

    it "can capture enemies at any of those squares" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4 Kh1"),(black,"pe6 pf3 pc2 pb5 Kh8")] "d4")) `shouldBe` sort [
        "e6","f5","f3","e2","c2","b3","b5","c6"]

    it "cannot budge teammates" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Nd4 pe6 pf3 pc2 pb5 Kh1")] "d4")) `shouldBe` sort [
        "f5","e2","b3","c6"]

  describe "A bishop" $ do

    it "can move in diagonal lines" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4 Kh1")] "d4")) `shouldBe` sort [
        "e5","f6","g7","h8",
        "e3","f2","g1",
        "c3","b2","a1",
        "c5","b6","a7"]

    it "can move in diagonal lines up to and including capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4 Kh1"),(black,"pg7 pf2 pc3 pb6 Kh8")] "d4")) `shouldBe` sort [
        "e5","f6","g7",
        "e3","f2",
        "c3",
        "c5","b6"]

    it "cannot budge teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Bd4 pg7 pf2 pc3 pb6 Kh1")] "d4")) `shouldBe` sort [
        "e5","f6",
        "e3",
        "c5"]

  describe "A queen" $ do

    it "can do whatever she damn well pleases" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Qd4 Kh1")] "d4")) `shouldBe` sort [
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
      sort (moveTargets (stdPossibleMoves [(white,"Qd4 Kh1"),(black,"pd7 pg4 pd2 pc4 pg7 pf2 pc3 pb6 Kh8")] "d4")) `shouldBe` sort [
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
      sort (moveTargets (stdPossibleMoves [(white,"Qd4 pd7 pg4 pd2 pc4 pg7 pf2 pc3 pb6 Kh1")] "d4")) `shouldBe` sort [
        -- horizontal (copied from rook)
        "d5","d6",
        "e4","f4",
        "d3",
        -- diagonal (copied from bishop)
        "e5","f6",
        "e3",
        "c5"]

  describe "A king" $ do

    it "can take one step anywhere" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Kd4")] "d4")) `shouldBe` sort [
        -- horizontal
        "d5",
        "e4",
        "d3",
        "c4",
        -- diagonal
        "e5",
        "e3",
        "c3",
        "c5"]

    it "can take one step anywhere and capture" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Kd4"),(black,"pe3 pd3 pc3")] "d4")) `shouldBe` sort [
        -- horizontal
        "d5",
        "e4",
        "d3",
        "c4",
        -- diagonal
        "e5",
        "e3",
        "c3",
        "c5"]

    it "cannot budge teammates" $ do 
      sort (moveTargets (stdPossibleMoves [(white,"Kd4 pe4 pc4 pe3 pc5")] "d4")) `shouldBe` sort [
        -- horizontal
        "d5",
        "d3",
        -- diagonal
        "e5",
        "c3"]
    
    describe "on a board where castling can happen either way" $ do
      
      it "can castle to either the left or right in ideal circumstances" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pd2 pe2 pf2 pg2 ph2"),(black,"Qc3 Qf3")] "d1"

        sort (moveTargets ms) `shouldBe` sort [
          "b1",
          "c1",
          "e1",
          "f1"
          ]
      
      it "can't castle when in check" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pe2 pf2 pg2 ph2"),(black,"Qd3")] "d1"
        sort (moveTargets ms) `shouldBe` sort [
          "c1",
          "e1"
          ]

      it "can't castle when rook is capturable" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pb2 pc2 pd2 pe2 pf2 pg2"),(black,"Ra3 Rh3")] "d1"
        sort (moveTargets ms) `shouldBe` sort [
          "c1",
          "e1"
          ]

      it "can't castle when the square the king leaps over is capturable" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pa2 pb2 pd2 pf2 pg2 ph2"),(black,"Rc3 Re3")] "d1"
        sort (moveTargets ms) `shouldBe` []

      it "can't castle when the king's target square is capturable" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pa2 pc2 pd2 pe2 pg2 ph2"),(black,"Rb3 Rf3")] "d1"
        sort (moveTargets ms) `shouldBe` sort [
          "c1",
          "e1"
          ]

      it "can castle when that lonely empty square by the far rook is capturable" $ do
        let 
          ms = stdPossibleMoves [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pd2 pe2 pf2 ph2"),(black,"Rg3")] "d1"
        sort (moveTargets ms) `shouldBe` sort [
          "b1",
          "c1",
          "e1",
          "f1"
          ]

  describe "Making sure castling is not possible once the involved parties have moved" $ do
    let
      pieces = [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pd2 pe2 pf2 pg2 ph2"),(black,"Qc3 Qf3")]
      b = b8x8 pieces
      Move (_,_) kingMoved = move b (loc "d1",loc "e1")
      Move (_,_) rookLeftMoved = move b (loc "a1",loc "b1")
      Move (_,_) rookRightMoved = move b (loc "h1",loc "g1")

    it "can't castle once the king has moved" $ do
      let 
        ms = stdPossibleMovesFromBoard kingMoved "e1"
      sort (moveTargets ms) `shouldBe` sort [
        "d1",
        "f1"
        ]

    it "can't castle left once the left rook has moved" $ do
      let 
        ms = stdPossibleMovesFromBoard rookLeftMoved "d1"
      sort (moveTargets ms) `shouldBe` sort [
        "c1",
        "e1",
        "f1"
        ]

    it "can't castle right once the right rook has moved" $ do
      let 
        ms = stdPossibleMovesFromBoard rookRightMoved "d1"
      sort (moveTargets ms) `shouldBe` sort [
        "b1",
        "c1",
        "e1"
        ]

  describe "The resulting board from castling" $ do
    let
      pieces = [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pd2 pe2 pf2 pg2 ph2"),(black,"Qc3 Qf3")]
      b = b8x8 pieces
      Move (_,_) bLeft = move b (loc "d1",loc "b1")
      Move (_,_) bRight = move b (loc "d1",loc "f1")

    it "should result in the castle jumping to the expected square when castling left" $ do
      fromRight (pieceAt (loc "b1") bLeft) `shouldBe` Just (Piece white King [loc "d1"])

    it "should move the left rook when castling left" $ do
      fromRight (pieceAt (loc "c1") bLeft) `shouldBe` Just (Piece white Rook [loc "a1"])

    it "should result in the castle jumping to the expected square when castling right" $ do
      fromRight (pieceAt (loc "f1") bRight) `shouldBe` Just (Piece white King [loc "d1"])

    it "should move the right rook when castling right" $ do
      fromRight (pieceAt (loc "e1") bRight) `shouldBe` Just (Piece white Rook [loc "h1"])

  describe "Pawn promotion" $ do
    let
      ms = fromRight (stdPossibleMoves [(white,"pd7")] "d7")

    it "should have a possible move for each piece the pawn could turn into" $ do
      length ms `shouldBe` 4
      
    -- TODO: Promotion when a pawn reaches their affinity

  describe "King protection" $ do

    it "means that a King cannot put himself in check" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Kd2"),(black,"Bh5 Ra3")] "d2")) `shouldBe` sort [
        "e1",
        "c1",
        "c2"]

    it "means a blocking teammate must not leave the defensive" $ do
      sort (moveTargets (stdPossibleMoves [(white,"Kd2 Qd4"),(black,"Rd8 Kh8")] "d4")) `shouldBe` sort [
        "d5","d6","d7","d8",
        "d3"]

  describe "Castling helpers" $ do
    describe "Calculating the line of site from a standard king" $ do
      let
        b = b8x8 [(white,"Kd1 Ra1 Rh1 pa2 pb2 pc2 pd2 pe2 pf2 pg2 ph2"),(black,"Qc3 Qf3")]
        toTheLeft = alglocs (lineOfSightEndingInUnmovedTeamRook b white (loc "d1") (-1,0))
        toTheRight = alglocs (lineOfSightEndingInUnmovedTeamRook b white (loc "d1") (1,0))

      it "should have a clear line of site to the left Rook" $ do
        toTheLeft `shouldBe` ["c1", "b1", "a1"]

      it "should have a clear line of site to the right Rook" $ do
        toTheRight `shouldBe` ["e1", "f1", "g1", "h1"]
        

  where 
    black = Team South "Black"
    white = Team North "White"
    b8x8 ps = newBoard 8 8 ps
    stdPossibleMoves ts al = stdPossibleMovesFromBoard (b8x8 ts) al
    stdPossibleMovesFromBoard b al = possibleMovesFromLocation b (fromAlgebraicLocation al) 1
    stdPossibleMovesWithHistory ts hist al = possibleMovesFromLocation (injectBoardHistory (b8x8 ts) hist) (fromAlgebraicLocation al) 1
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

algloc :: Location -> String
algloc = toAlgebraicLocation

alglocs :: [Location] -> [String]
alglocs [] = []
alglocs (x:xs) = algloc x : alglocs xs

injectBoardHistory :: Board -> [(String,[String])] -> Board
injectBoardHistory b [] = b
injectBoardHistory (Board m capt bs) ((al,hist):hs) = 
  let updateOrFail p = if p == Nothing then error "Invalid location" else Just (Just (injectPieceHistory (fromJust p) hist))
  in injectBoardHistory (Board (Map.update updateOrFail (loc al) m) capt bs) hs 

injectPieceHistory :: Piece -> [String] -> Piece
injectPieceHistory (Piece t c _) hs = Piece t c (locs hs)


