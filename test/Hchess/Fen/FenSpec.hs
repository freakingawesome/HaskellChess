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
      gameMove0 = newStandardGame
      gameMove1 = mv gameMove0 "e2" "e4"
      gameMove2 = mv gameMove1 "c7" "c5"
      gameMove3 = mv gameMove2 "g1" "f3"

    it "should have the correct FEN output for a new game" $ do
      toFen gameMove0 `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    it "should have the correct FEN output after move e4" $ do
      toFen gameMove1 `shouldBe` "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    it "should have the correct FEN output after move c5" $ do
      toFen gameMove2 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

    it "should have the correct FEN output after move Nf3" $ do
      toFen gameMove3 `shouldBe` "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

