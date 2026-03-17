module DarkmoonFaireSpec (spec) where

import Test.Hspec

import App
import Event.DarkmoonFaire
import Graph
import Session
import Solver
import Types

defaultCfg :: Int -> Config
defaultCfg ticketCount =
  Config
    { eventName = DarkmoonFaire
    , inputMode = CliMode
    , tickets = ticketCount
    , valuation = defaultValuation
    , debugEnabled = False
    }

starHeavyCfg :: Int -> Config
starHeavyCfg ticketCount =
  Config
    { eventName = DarkmoonFaire
    , inputMode = CliMode
    , tickets = ticketCount
    , valuation =
        Valuation
          { valueExp = 0
          , valueMinorStar = 200
          , valueMajorStar = 300
          , valueArclightEnergy = 0
          , valueRareCore = 0
          }
    , debugEnabled = False
    }

dmfGraph :: Graph
dmfGraph =
  fst (runApp (defaultCfg 34900) (buildGraphM darkmoonPayloads))

spec :: Spec
spec = do
  describe "Darkmoon Faire regression" $ do
    it "builds the expected 28-node graph" $
      length dmfGraph `shouldBe` 28

    it "has no claimed-by-default nodes in payload data" $
      claimedIds dmfGraph `shouldBe` []

    it "accepts an empty claimed traversal" $ do
      let (result, _) = runApp (defaultCfg 34900) (runSessionM dmfGraph [])
      case result of
        SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution -> do
          accepted `shouldBe` []
          nextNodes `shouldBe` ["A1","A2","A3","A4"]
          rewardTotal `shouldBe` zeroReward
          score `shouldBe` 0
          spent `shouldBe` 0
          remaining `shouldBe` 34900
          resultCost solution `shouldSatisfy` (<= 34900)
          resultPath solution `shouldNotBe` []
          reachable `shouldSatisfy` elem "H2"
        _ ->
          expectationFailure "Expected SessionValid for empty traversal"

    it "accepts a screenshot-like early traversal A1 -> B1 -> C2" $ do
      let claimed = ["A1","B1","C2"]
          (result, _) = runApp (defaultCfg 34900) (runSessionM dmfGraph claimed)
      case result of
        SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution -> do
          accepted `shouldBe` claimed
          spent `shouldBe` 1800
          remaining `shouldBe` 33100
          rewardTotal `shouldBe`
            Reward
              { rewardGold = 200
              , rewardExp = 1500
              , rewardMinorStars = 0
              , rewardMajorStars = 0
              , rewardArclightEnergy = 0
              , rewardRareCores = 0
              }
          score `shouldBe` 230
          nextNodes `shouldSatisfy` elem "D1"
          reachable `shouldSatisfy` elem "H1"
          resultCost solution `shouldSatisfy` (<= 33100)
        _ ->
          expectationFailure "Expected SessionValid for A1 B1 C2"

    it "accepts a screenshot-like branch A2 -> B2 -> C3" $ do
      let claimed = ["A2","B2","C3"]
          (result, _) = runApp (defaultCfg 34900) (runSessionM dmfGraph claimed)
      case result of
        SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution -> do
          accepted `shouldBe` claimed
          spent `shouldBe` 2200
          remaining `shouldBe` 32700
          rewardTotal `shouldBe`
            Reward
              { rewardGold = 0
              , rewardExp = 7500
              , rewardMinorStars = 0
              , rewardMajorStars = 0
              , rewardArclightEnergy = 0
              , rewardRareCores = 0
              }
          score `shouldBe` 150
          nextNodes `shouldSatisfy` elem "D2"
          reachable `shouldSatisfy` elem "H2"
          resultCost solution `shouldSatisfy` (<= 32700)
        _ ->
          expectationFailure "Expected SessionValid for A2 B2 C3"

    it "changes optimal score under a star-heavy valuation profile" $ do
      let (resultDefault, _) = runApp (defaultCfg 34900) (runSessionM dmfGraph [])
          (resultStars, _)   = runApp (starHeavyCfg 34900) (runSessionM dmfGraph [])
      case (resultDefault, resultStars) of
        ( SessionValid _ _ _ _ _ _ _ solutionDefault
          , SessionValid _ _ _ _ _ _ _ solutionStars ) -> do
              resultScore solutionStars `shouldNotBe` resultScore solutionDefault
        _ ->
          expectationFailure "Expected both sessions to be valid"

    it "keeps solver result within remaining tickets on the real event graph" $ do
      let claimed = ["A1","B1","C2"]
          (result, _) = runApp (defaultCfg 34900) (runSessionM dmfGraph claimed)
      case result of
        SessionValid _ _ _ _ _ _ remaining solution ->
          resultCost solution `shouldSatisfy` (<= remaining)
        _ ->
          expectationFailure "Expected SessionValid"