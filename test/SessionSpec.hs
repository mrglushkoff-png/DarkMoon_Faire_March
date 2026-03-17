module SessionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import App
import Graph
import Session
import Types

testConfig :: Config
testConfig =
  Config
    { eventName = DarkmoonFaire
    , inputMode = CliMode
    , tickets = 1000
    , valuation = defaultValuation
    , debugEnabled = False
    }

mkConfig :: Int -> Config
mkConfig ticketCount =
  testConfig { tickets = ticketCount }

rGold :: Int -> Reward
rGold n = zeroReward { rewardGold = n }

sessionGraph :: Graph
sessionGraph =
  [ Node "A1" 1 100 (rGold 50) [] False
  , Node "A2" 1 100 (rGold 0)  [] False
  , Node "B1" 2 200 (rGold 80) ["A1","A2"] False
  , Node "B2" 2 150 (rGold 30) ["A2"] False
  ]

newtype ArbBudget = ArbBudget Int
  deriving Show

instance Arbitrary ArbBudget where
  arbitrary = ArbBudget <$> chooseInt (0, 2000)

newtype ArbSessionIds = ArbSessionIds [NodeId]
  deriving Show

instance Arbitrary ArbSessionIds where
  arbitrary = ArbSessionIds <$> sublistOf (map nodeId sessionGraph)

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

  describe "QuickCheck session invariants" $ do
    it "valid sessions only return known accepted ids" $
      property $ \(ArbBudget budget) (ArbSessionIds xs) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (runSessionM sessionGraph xs)
        in case result of
             SessionInvalid _ ->
               True
             SessionValid accepted _ _ _ _ _ _ _ ->
               all (`elem` map nodeId sessionGraph) accepted

    it "spent plus remaining equals configured tickets on valid sessions" $
      property $ \(ArbBudget budget) (ArbSessionIds xs) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (runSessionM sessionGraph xs)
        in case result of
             SessionInvalid _ ->
               True
             SessionValid _ _ _ _ _ spent remaining _ ->
               spent + remaining == budget

    it "next nodes never overlap accepted nodes" $
      property $ \(ArbBudget budget) (ArbSessionIds xs) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (runSessionM sessionGraph xs)
        in case result of
             SessionInvalid _ ->
               True
             SessionValid accepted nextNodes _ _ _ _ _ _ ->
               S.null (S.fromList accepted `S.intersection` S.fromList nextNodes)

    it "accepted ids in valid sessions are claimable in sequence" $
      property $ \(ArbBudget budget) (ArbSessionIds xs) ->
        let cfg = mkConfig budget
            (result, _) = runApp cfg (runSessionM sessionGraph xs)
        in case result of
             SessionInvalid _ ->
               True
             SessionValid accepted _ _ _ _ _ _ _ ->
               validSequence sessionGraph accepted

showTag :: SessionResult -> String
showTag (SessionInvalid e) = "SessionInvalid " ++ show e
showTag SessionValid{}     = "SessionValid"

validSequence :: Graph -> [NodeId] -> Bool
validSequence graph =
  go S.empty
  where
    go _ [] = True
    go acceptedSet (nid:nids) =
      case lookupNode nid graph of
        Nothing ->
          False
        Just node ->
             nodeId node `S.notMember` acceptedSet
          && claimable acceptedSet node
          && go (S.insert nid acceptedSet) nids