module SolverSpec (spec) where

import Test.Hspec

import App
import Solver
import Types

tinyValuation :: Valuation
tinyValuation =
  Valuation
    { valueExp = 0
    , valueMinorStar = 90
    , valueMajorStar = 120
    , valueArclightEnergy = 0
    , valueRareCore = 0
    }

testConfig :: Config
testConfig =
  Config
    { eventName = DarkmoonFaire
    , tickets = 1000
    , valuation = tinyValuation
    , debugEnabled = False
    }

rGold :: Int -> Reward
rGold n = zeroReward { rewardGold = n }

simpleGraph :: Graph
simpleGraph =
  [ Node "A1" 1 100 (rGold 50) [] False
  , Node "A2" 1 200 (rGold 70) [] False
  ]

branchGraph :: Graph
branchGraph =
  [ Node "A1" 1 100 (rGold 0) [] True
  , Node "B1" 2 100 (rGold 30) ["A1"] False
  , Node "B2" 2 100 (rGold 80) ["A1"] False
  ]

tieGraph :: Graph
tieGraph =
  [ Node "A1" 1 100 (rGold 50) [] False
  , Node "A2" 1 200 (rGold 50) [] False
  ]

spec :: Spec
spec = do
  describe "solveRemainingM" $ do
    it "returns emptyResult when nothing is affordable" $ do
      let cfg = testConfig { tickets = 0 }
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      result `shouldBe` emptyResult

    it "picks the better available node" $ do
      let cfg = testConfig { tickets = 200 }
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      resultPath result `shouldBe` ["A2"]
      resultScore result `shouldBe` 70

    it "picks the better branch under the same parent" $ do
      let cfg = testConfig { tickets = 200 }
          (result, _) = runApp cfg (solveRemainingM branchGraph)
      resultPath result `shouldBe` ["B2"]
      resultScore result `shouldBe` 80

    it "breaks equal-score ties toward lower cost" $ do
      let cfg = testConfig { tickets = 200 }
          (result, _) = runApp cfg (solveRemainingM tieGraph)
      resultPath result `shouldBe` ["A1"]
      resultCost result `shouldBe` 100

    it "never exceeds remaining ticket budget" $ do
      let cfg = testConfig { tickets = 150 }
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      resultCost result `shouldSatisfy` (<= 150)