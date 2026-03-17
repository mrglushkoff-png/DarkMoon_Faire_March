module Render
  ( renderGraph
  , renderGraphTable
  , renderLogs
  , renderValidationFailure
  , renderTraversalSummary
  , renderSolverSummary
  , renderRewardCompact
  , renderParents
  ) where

import Data.List (groupBy, sortOn)

import Graph
import Solver
import Types

renderGraph :: Graph -> [String]
renderGraph graph =
  "Graph:" : map renderRow (graphRows graph)

renderGraphTable :: Graph -> [String]
renderGraphTable graph =
  "Node table:" :
  [ nodeId node
      ++ " | cost=" ++ show (cost node)
      ++ " | reward=" ++ renderRewardCompact (reward node)
      ++ " | parents=" ++ renderParents (parents node)
  | node <- graph
  ]

renderLogs :: [LogEntry] -> [String]
renderLogs entries =
  case entries of
    [] -> []
    _  -> concatMap renderGroup grouped
  where
    grouped =
      groupBy samePhase (sortOn logPhase entries)

    samePhase a b =
      logPhase a == logPhase b

    renderGroup xs =
      case xs of
        [] -> []
        e:_ ->
          "" : ("[DEBUG] " ++ show (logPhase e))
             : map (("  - " ++) . logMessage) xs

renderValidationFailure :: ValidationError -> [String]
renderValidationFailure err =
  [ ""
  , "Traversal validation failed:"
  , show err
  ]

renderTraversalSummary
  :: [NodeId]
  -> [NodeId]
  -> [NodeId]
  -> Reward
  -> Double
  -> Int
  -> Int
  -> Double
  -> Double
  -> [String]
renderTraversalSummary accepted next reachable rewardTotal score spent remaining leftoverTicketValue totalOutcomeScore =
  [ ""
  , "Traversal is valid."
  , "Accepted claimed nodes: " ++ unwords accepted
  , "Currently claimable next nodes: " ++ unwords next
  , "Remaining reachable nodes: " ++ unwords reachable
  , ""
  , "Claimed reward total:"
  , show rewardTotal
  , "  = " ++ renderRewardCompact rewardTotal
  , ""
  , "Claimed reward score:"
  , show score
  , ""
  , "Historical claimed ticket cost:"
  , show spent
  , ""
  , "Available tickets now:"
  , show remaining
  , ""
  , "Leftover ticket value at event end:"
  , show leftoverTicketValue
  , ""
  , "Total outcome value:"
  , show totalOutcomeScore
  ]

renderSolverSummary :: SearchResult -> [String]
renderSolverSummary result =
  [ ""
  , "Best remaining path:"
  , show (resultPath result)
  , ""
  , "Best remaining reward:"
  , show (resultReward result)
  , "  = " ++ renderRewardCompact (resultReward result)
  , ""
  , "Best remaining ticket cost:"
  , show (resultCost result)
  , ""
  , "Best remaining score:"
  , show (resultScore result)
  ]

renderRow :: [Node] -> String
renderRow = unwords . map nodeId

renderRewardCompact :: Reward -> String
renderRewardCompact r =
  unwords (filter (not . null)
    [ part "G"      (rewardGold r)
    , part "EXP"    (rewardExp r)
    , part "Troop"  (rewardMinorStars r)
    , part "Leader" (rewardMajorStars r)
    , part "AE"     (rewardArclightEnergy r)
    , part "RC"     (rewardRareCores r)
    ])
  where
    part _ 0 = ""
    part s n = show n ++ "*" ++ s

renderParents :: [NodeId] -> String
renderParents [] = "-"
renderParents xs = unwords xs