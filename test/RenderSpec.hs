module RenderSpec (spec) where

import Test.Hspec

import Render
import Solver
import Types

sampleReward :: Reward
sampleReward =
  Reward
    { rewardGold = 150
    , rewardExp = 5000
    , rewardMinorStars = 1
    , rewardMajorStars = 0
    , rewardArclightEnergy = 500
    , rewardEpicCores = 1
    }

sampleResult :: SearchResult
sampleResult =
  SearchResult
    { resultPath = ["B2","C3","D3"]
    , resultReward = sampleReward
    , resultCost = 4200
    , resultScore = 999.0
    }

sampleLogs :: [LogEntry]
sampleLogs =
  [ LogEntry GraphBuild "built graph"
  , LogEntry GraphBuild "row count ok"
  , LogEntry Solving "memo hits = 10"
  ]

spec :: Spec
spec = do
  describe "renderLogs" $ do
    it "renders empty logs as empty output" $
      renderLogs [] `shouldBe` []

    it "groups logs by phase" $
      renderLogs sampleLogs `shouldBe`
        [ ""
        , "[DEBUG] GraphBuild"
        , "  - built graph"
        , "  - row count ok"
        , ""
        , "[DEBUG] Solving"
        , "  - memo hits = 10"
        ]

  describe "renderValidationFailure" $ do
    it "renders validation failures clearly" $
      renderValidationFailure (UnknownNodeId "Z9") `shouldBe`
        [ ""
        , "Traversal validation failed:"
        , "UnknownNodeId \"Z9\""
        ]

  describe "renderTraversalSummary" $ do
    it "renders traversal summary lines" $
      renderTraversalSummary
        ["A2","B2","C2"]
        ["A1","A3","B1"]
        ["A1","A3","A4","B1","B3","C1"]
        sampleReward
        360.0
        2200
        1000
        0.0
        1359.0
      `shouldBe`
        [ ""
        , "Traversal is valid."
        , "Accepted claimed nodes: A2 B2 C2"
        , "Currently claimable next nodes: A1 A3 B1"
        , "Remaining reachable nodes: A1 A3 A4 B1 B3 C1"
        , ""
        , "Claimed reward total:"
        , show sampleReward
        , "  = 150*G 5000*EXP 1*Troop 500*AE 1*EC"
        , ""
        , "Claimed reward score:"
        , "360.0"
        , ""
        , "Historical claimed ticket cost:"
        , "2200"
        , ""
        , "Available tickets now:"
        , "1000"
        , ""
        , "Leftover ticket value at event end:"
        , "0.0"
        , ""
        , "Total outcome value:"
        , "1359.0"
        ]

  describe "renderSolverSummary" $ do
    it "renders solver result lines" $
      renderSolverSummary sampleResult `shouldBe`
        [ ""
        , "Best remaining path:"
        , show ["B2","C3","D3"]
        , ""
        , "Best remaining reward:"
        , show sampleReward
        , "  = 150*G 5000*EXP 1*Troop 500*AE 1*EC"
        , ""
        , "Best remaining ticket cost:"
        , "4200"
        , ""
        , "Best remaining score:"
        , "999.0"
        ]