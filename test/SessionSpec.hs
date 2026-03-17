module SessionSpec (spec) where

import Test.Hspec

import App
import Graph
import Session
import Types

testConfig :: Config
testConfig =
  Config
    { eventName = DarkmoonFaire
    , tickets = 1000
    , valuation = defaultValuation
    , debugEnabled = False
    }

rGold :: Int -> Reward
rGold n = zeroReward { rewardGold = n }

sessionGraph :: Graph
sessionGraph =
  [ Node "A1" 1 100 (rGold 50) [] False
  , Node "A2" 1 100 (rGold 0)  [] False
  , Node "B1" 2 200 (rGold 80) ["A1","A2"] False
  , Node "B2" 2 150 (rGold 30) ["A2"] False
  ]

spec :: Spec
spec = do
  describe "runSessionM" $ do
    it "returns SessionInvalid for a disconnected traversal" $ do
      let (result, _) = runApp testConfig (runSessionM sessionGraph ["B1"])
      case result of
        SessionInvalid (DisconnectedClaim "B1") -> pure ()
        _ -> expectationFailure ("Expected disconnected B1, got " ++ showTag result)

    it "returns SessionValid for a valid traversal" $ do
      let (result, _) = runApp testConfig (runSessionM sessionGraph ["A2","B2"])
      case result of
        SessionValid accepted nextNodes reachable rewardTotal _score spent remaining _solution -> do
          accepted `shouldBe` ["A2","B2"]
          nextNodes `shouldSatisfy` elem "A1"
          reachable `shouldSatisfy` elem "A1"
          rewardTotal `shouldBe` addReward (rGold 0) (rGold 30)
          spent `shouldBe` 250
          remaining `shouldBe` 750
        _ ->
          expectationFailure ("Expected SessionValid, got " ++ showTag result)

    it "marks future high-value nodes as reachable, not only immediate frontier" $ do
      let (result, _) = runApp testConfig (runSessionM sessionGraph ["A2"])
      case result of
        SessionValid _ _ reachable _ _ _ _ _ -> do
          reachable `shouldSatisfy` elem "B1"
          reachable `shouldSatisfy` elem "B2"
        _ ->
          expectationFailure ("Expected SessionValid, got " ++ showTag result)

showTag :: SessionResult -> String
showTag (SessionInvalid e) = "SessionInvalid " ++ show e
showTag SessionValid{}     = "SessionValid"