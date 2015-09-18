module Hchess.Game.GameSpec where

import SpecHelper
import TestUtil
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.List(sort)
import Data.Either.Unwrap

spec :: Spec
spec = do
  describe "Team management" $ do
    it "should repeat the team order when cycling through" $ do
      take 10 (getTurns [1,2,3]) `shouldBe` [1,2,3,1,2,3,1,2,3,1]

    it "should get an appropriate team count for one team" $ do
      teamCount (getTurns [1]) `shouldBe` 1

    it "should get an appropriate team count for two teams" $ do
      teamCount (getTurns [1,2]) `shouldBe` 2

    it "should get an appropriate team count for many teams" $ do
      teamCount (getTurns [1,2,3,4,5,6]) `shouldBe` 6

    it "should remove teams appropriately" $ do
      take 10 (removeTeam (getTurns [1,2,3]) 2) `shouldBe` [1,3,1,3,1,3,1,3,1,3]

    it "should be able to change teams back into turns at the first position" $ do
      turnsToTeams (getTurns [1,2,3]) `shouldBe` [1,2,3]

    it "should be able to change teams back into turns at other positions" $ do
      turnsToTeams (tail (getTurns [1,2,3])) `shouldBe` [2,3,1]

  describe "A new standard game" $ do
    it "errors out when moving from a non-existent square" $ do
      performMove newStandardGame (loc "z3",loc "z4") Nothing `shouldBe` Left "Invalid location"

    it "errors out when moving from an empty square" $ do
      performMove newStandardGame (loc "b4",loc "a4") Nothing `shouldBe` Left "The source square is empty"

    it "only allows white to move first" $ do
      performMove newStandardGame (loc "a7",loc "a6") Nothing `shouldBe` Left "It is not Black's turn"

    it "should not allow an impossible move" $ do
      performMove newStandardGame (loc "a2",loc "a5") Nothing `shouldBe` Left "Illegal move"

    it "should not allow an incapacitated piece to move" $ do
      performMove newStandardGame (loc "a1",loc "a2") Nothing `shouldBe` Left "This piece is currently incapacitated"

    it "should allow a possible move" $ do
      isRight (performMove newStandardGame (loc "a2",loc "a4") Nothing) `shouldBe` True

    it "should not allow specification of a promotion character when it isn't applicable" $ do
      performMove newStandardGame (loc "a2",loc "a4") (Just Queen) `shouldBe` Left "Promotion is not valid here"

  describe "The game after the first white move" $ do
    let
      Right g = performMove newStandardGame (loc "a2",loc "a4") Nothing
      Game b teams _ _ = g
      Right (Just (Piece t c)) = pieceAt (loc "a4") b

    it "should now be black's move" $ do
      head teams `shouldBe` black

    it "should have the new board placement" $ do
      (t,c) `shouldBe` (white,Pawn)

  describe "A move requiring promotion" $ do
    let
      g = newGame 8 8 [(white,"Ka1 pb7"),(black,"Kh7")]

    it "should require specification of a promotion character" $ do
      performMove g (loc "b7",loc "b8") Nothing `shouldBe` Left "You must specify a character for promotion"

    it "should not allow an invalid character for promotion" $ do
      performMove g (loc "b7",loc "b8") (Just King) `shouldBe` Left "Invalid character for promotion"

    it "should allow a promotion" $ do
      let
        Right (Game b _ _ _) = performMove g (loc "b7",loc "b8") (Just Queen)

      getCharacter (fromJust (fromRight (pieceAt (loc "b8") b))) `shouldBe` Queen

  describe "Checking for stalemate" $ do
    it "should not find a stalemate if it ain't so" $ do
      isStalemate (newGame 8 8 [(white,"Ka1 pb7"),(black,"Kh7")]) `shouldBe` False

    it "should find a stalemate if the current team can't move" $ do
      isStalemate (newGame 8 8 [(white,"Kh8"),(black,"Kf7 Qg6")]) `shouldBe` True

  describe "Checking for checkmate" $ do
    let
      cm = (newGame 8 8 [(white,"Kh8"),(black,"Kf7 Qh6")])

    it "should not find a checkmate if it ain't so" $ do
      isCheckmate (newGame 8 8 [(white,"Ka1 pb7"),(black,"Kh7")]) `shouldBe` False

    it "should find a checkmate if the current team can't move and king is in check" $ do
      isCheckmate cm `shouldBe` True

    it "should not count a checkmate as a stalemate" $ do
      isStalemate cm `shouldBe` False

