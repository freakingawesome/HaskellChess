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

  describe "A new standard game" $ do
    it "should have the correct FEN output" $ do
      toFen newStandardGame `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

