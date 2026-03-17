module Render
  ( renderGraph
  , renderLogs
  , renderValidationFailure
  , renderTraversalSummary
  , renderSolverSummary
  ) where

import Data.List (groupBy, sortOn)

import Graph
import Solver
import Types

renderGraph :: Graph -> [String]
renderGraph graph =
  "Graph:" : map renderRow (graphRows graph)

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
  -> [String]
renderTraversalSummary accepted next reachable rewardTotal score spent remaining =
  [ ""
  , "Traversal is valid."
  , "Accepted claimed nodes: " ++ unwords accepted
  , "Currently claimable next nodes: " ++ unwords next
  , "Remaining reachable nodes: " ++ unwords reachable
  , ""
  , "Claimed reward total:"
  , show rewardTotal
  , ""
  , "Claimed reward score:"
  , show score
  , ""
  , "Claimed ticket cost:"
  , show spent
  , ""
  , "Remaining tickets:"
  , show remaining
  ]

renderSolverSummary :: SearchResult -> [String]
renderSolverSummary result =
  [ ""
  , "Best remaining path:"
  , show (resultPath result)
  , ""
  , "Best remaining reward:"
  , show (resultReward result)
  , ""
  , "Best remaining ticket cost:"
  , show (resultCost result)
  , ""
  , "Best remaining score:"
  , show (resultScore result)
  ]

renderRow :: [Node] -> String
renderRow = unwords . map nodeId