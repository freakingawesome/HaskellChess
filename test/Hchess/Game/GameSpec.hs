module Hchess.Game.GameSpec where

import SpecHelper
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.List(sort)

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
