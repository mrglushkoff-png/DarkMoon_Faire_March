module SolverSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import App
import Graph
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

mkConfig :: Int -> Config
mkConfig ticketCount =
  Config
    { eventName = DarkmoonFaire
    , inputMode = CliMode
    , tickets = ticketCount
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

solverGraph :: Graph
solverGraph =
  [ Node "A1" 1 100 (rGold 10) [] False
  , Node "A2" 1 120 (rGold 15) [] False
  , Node "B1" 2 130 (rGold 40) ["A1"] False
  , Node "B2" 2 140 (rGold 45) ["A2"] False
  , Node "C1" 3 150 (rGold 80) ["B1","B2"] False
  ]

newtype ArbBudget = ArbBudget Int
  deriving Show

instance Arbitrary ArbBudget where
  arbitrary = ArbBudget <$> chooseInt (0, 1000)

spec :: Spec
spec = do
  describe "solveRemainingM" $ do
    it "returns emptyResult when nothing is affordable" $ do
      let cfg = mkConfig 0
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      result `shouldBe` emptyResult

    it "picks the better available node" $ do
      let cfg = mkConfig 200
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      resultPath result `shouldBe` ["A2"]
      resultScore result `shouldBe` 70

    it "picks the better branch under the same parent" $ do
      let cfg = mkConfig 200
          (result, _) = runApp cfg (solveRemainingM branchGraph)
      resultPath result `shouldBe` ["B2"]
      resultScore result `shouldBe` 80

    it "breaks equal-score ties toward lower cost" $ do
      let cfg = mkConfig 200
          (result, _) = runApp cfg (solveRemainingM tieGraph)
      resultPath result `shouldBe` ["A1"]
      resultCost result `shouldBe` 100

    it "never exceeds remaining ticket budget" $ do
      let cfg = mkConfig 150
          (result, _) = runApp cfg (solveRemainingM simpleGraph)
      resultCost result `shouldSatisfy` (<= 150)

  describe "QuickCheck solver invariants" $ do
    it "result cost never exceeds configured ticket budget" $
      property $ \(ArbBudget budget) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (solveRemainingM solverGraph)
        in resultCost result <= budget

    it "result path contains no duplicate node ids" $
      property $ \(ArbBudget budget) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (solveRemainingM solverGraph)
            path = resultPath result
        in length path == S.size (S.fromList path)

    it "result path is claimable in sequence" $
      property $ \(ArbBudget budget) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (solveRemainingM solverGraph)
        in validSequence solverGraph (resultPath result)

validSequence :: Graph -> [NodeId] -> Bool
validSequence graph =
  go S.empty
  where
    go _ [] = True
    go claimedSet (nid:nids) =
      case lookupNode nid graph of
        Nothing ->
          False
        Just node ->
             nodeId node `S.notMember` claimedSet
          && claimable claimedSet node
          && go (S.insert nid claimedSet) nids