module Hchess.Fen.FenSpec where

import SpecHelper
import TestUtil
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.List(sort)
import Data.Either.Unwrap
import qualified Data.HashSet as HS
import Hchess.Fen

spec :: Spec
spec = do

  describe "The moves based off the wikipedia FEN entry" $ do
    -- the moves here are based off the wikipedia article:
    -- https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
    let
      mv game from to = fromRight $ performMove game ((fromAlgebraicLocation from),(fromAlgebraicLocation to)) Nothing
      wikipediaMove0 = newStandardGame
      wikipediaMove1 = mv wikipediaMove0 "e2" "e4"
      wikipediaMove2 = mv wikipediaMove1 "c7" "c5"
      wikipediaMove3 = mv wikipediaMove2 "g1" "f3"
      myMove4        = mv wikipediaMove3 "e7" "e5"
      myMove5        = mv myMove4 "f3" "e5"
      myMove6        = mv myMove5 "e8" "e7"
      myMove7        = mv myMove6 "e1" "e2"

    it "should have the correct FEN output for a new game" $ do
      toFen wikipediaMove0 `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    it "should have the correct FEN output after move e4" $ do
      toFen wikipediaMove1 `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    it "should have the correct FEN output after move c5" $ do
      toFen wikipediaMove2 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

    it "should have the correct FEN output after move Nf3" $ do
      toFen wikipediaMove3 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

    it "should have the correct FEN output after move e5" $ do
      toFen myMove4 `shouldBe` "rnbqkbnr/pp1p1ppp/8/2p1p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq e6 0 3"

    it "should have the correct FEN output after capture at e5" $ do
      toFen myMove5 `shouldBe` "rnbqkbnr/pp1p1ppp/8/2p1N3/4P3/8/PPPP1PPP/RNBQKB1R b KQkq - 0 3"

    it "should have the correct FEN output (no castling for black) after king moves to e7" $ do
      toFen myMove6 `shouldBe` "rnbq1bnr/pp1pkppp/8/2p1N3/4P3/8/PPPP1PPP/RNBQKB1R w KQ - 1 4"

    it "should have the correct FEN output (no castling for either) after king moves to e2" $ do
      toFen myMove7 `shouldBe` "rnbq1bnr/pp1pkppp/8/2p1N3/4P3/8/PPPPKPPP/RNBQ1B1R b - - 2 4"

