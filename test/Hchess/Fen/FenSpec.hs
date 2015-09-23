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
      fenMove0 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

      wikipediaMove1 = mv wikipediaMove0 "e2" "e4"
      fenMove1 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

      wikipediaMove2 = mv wikipediaMove1 "c7" "c5"
      fenMove2 = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

      wikipediaMove3 = mv wikipediaMove2 "g1" "f3"
      fenMove3 = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

      myMove4 = mv wikipediaMove3 "e7" "e5"
      fenMove4 = "rnbqkbnr/pp1p1ppp/8/2p1p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq e6 0 3"

      myMove5 = mv myMove4 "f3" "e5"
      fenMove5 = "rnbqkbnr/pp1p1ppp/8/2p1N3/4P3/8/PPPP1PPP/RNBQKB1R b KQkq - 0 3"

      myMove6 = mv myMove5 "e8" "e7"
      fenMove6 = "rnbq1bnr/pp1pkppp/8/2p1N3/4P3/8/PPPP1PPP/RNBQKB1R w KQ - 1 4"

      myMove7 = mv myMove6 "e1" "e2"
      fenMove7 = "rnbq1bnr/pp1pkppp/8/2p1N3/4P3/8/PPPPKPPP/RNBQ1B1R b - - 2 4"

    describe "should have the correct FEN output" $ do

      it "for a new game" $ do
        toFen wikipediaMove0 `shouldBe` fenMove0

      it "after move e4" $ do
        toFen wikipediaMove1 `shouldBe` fenMove1

      it "after move c5" $ do
        toFen wikipediaMove2 `shouldBe` fenMove2

      it "after move Nf3" $ do
        toFen wikipediaMove3 `shouldBe` fenMove3

      it "after move e5" $ do
        toFen myMove4 `shouldBe` fenMove4

      it "after capture at e5" $ do
        toFen myMove5 `shouldBe` fenMove5

      it "after king moves to e7 (no castling for black)" $ do
        toFen myMove6 `shouldBe` fenMove6

      it "after king moves to e2 (no castling for either)" $ do
        toFen myMove7 `shouldBe` fenMove7

