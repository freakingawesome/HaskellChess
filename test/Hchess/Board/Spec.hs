module Hchess.Board.Spec
    (boardSpecs)
where
import Hchess.Board 
import Data.Map (size)
import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)

boardSpecs :: IO ()
boardSpecs = hspec $ do
  describe "Board size" $ do
    it "should multiple width and height" $ do
      boardSize (emptyBoard 7 3) `shouldBe` 63
  where boardSize (Board x _) = size x


