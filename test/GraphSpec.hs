module GraphSpec (spec) where

import Test.Hspec

import App
import Graph
import Types

testConfig :: Config
testConfig =
  Config
    { eventName = DarkmoonFaire
    , tickets = 34900
    , valuation = defaultValuation
    , debugEnabled = False
    }

testGraph :: Graph
testGraph =
  fst (runApp testConfig (buildGraphM (replicate (sum rowSizes) defaultPayload)))

spec :: Spec
spec = do
  describe "graph topology" $ do
    it "builds the expected number of nodes" $
      length testGraph `shouldBe` sum rowSizes

    it "builds 8 rows with 4-3 alternation" $
      map length (graphRows testGraph) `shouldBe` [4,3,4,3,4,3,4,3]

    it "wires 4->3 rows correctly" $ do
      fmap parents (lookupNode "B1" testGraph) `shouldBe` Just ["A1","A2"]
      fmap parents (lookupNode "B2" testGraph) `shouldBe` Just ["A2","A3"]
      fmap parents (lookupNode "B3" testGraph) `shouldBe` Just ["A3","A4"]

    it "wires 3->4 rows correctly" $ do
      fmap parents (lookupNode "C1" testGraph) `shouldBe` Just ["B1"]
      fmap parents (lookupNode "C2" testGraph) `shouldBe` Just ["B1","B2"]
      fmap parents (lookupNode "C3" testGraph) `shouldBe` Just ["B2","B3"]
      fmap parents (lookupNode "C4" testGraph) `shouldBe` Just ["B3"]

  describe "claimability" $ do
    it "initially exposes the first row" $
      claimableNow testGraph [] `shouldBe` ["A1","A2","A3","A4"]

    it "makes B1 and B2 claimable after A2" $
      claimableNow testGraph ["A2"] `shouldBe` ["A1","A3","A4","B1","B2"]

  describe "validation" $ do
    it "accepts a valid simple traversal" $ do
      let result = fst (runApp testConfig (validateClaimedM testGraph ["A2","B2","C2"]))
      result `shouldBe` Right ["A2","B2","C2"]

    it "rejects an unknown node" $ do
      let result = fst (runApp testConfig (validateClaimedM testGraph ["Z9"]))
      result `shouldBe` Left (UnknownNodeId "Z9")

    it "rejects duplicates" $ do
      let result = fst (runApp testConfig (validateClaimedM testGraph ["A1","A1"]))
      result `shouldBe` Left (DuplicateNodeId "A1")

    it "rejects disconnected claims" $ do
      let result = fst (runApp testConfig (validateClaimedM testGraph ["D3"]))
      result `shouldBe` Left (DisconnectedClaim "D3")

  describe "reachability" $ do
    it "includes future-reachable nodes, not just immediate ones" $ do
      let reachable = reachableIds testGraph ["A2"]
      reachable `shouldSatisfy` elem "B2"
      reachable `shouldSatisfy` elem "C2"
      reachable `shouldSatisfy` elem "D2"