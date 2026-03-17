module Main where

import App
import Config
import EventLoader
import Render
import Session
import Types

emit :: [String] -> IO ()
emit = putStr . unlines

main :: IO ()
main = do
  emit ["Darkmoon Faire Solver"]

  cfg <- getConfig

  let ev = eventName cfg
      (graph, loadLogs) = runApp cfg (loadEventGraphM ev)

  emit $
       renderLogs loadLogs
    ++ [""]
    ++ ["Loaded event: " ++ show ev]
    ++ [""]
    ++ renderGraph graph
    ++ [""]
    ++ ["Enter claimed nodes as space-separated ids:"]

  claimed <- words <$> getLine

  let (sessionResult, sessionLogs) = runApp cfg (runSessionM graph claimed)

  emit (renderLogs sessionLogs)

  case sessionResult of
    SessionInvalid err ->
      emit (renderValidationFailure err ++ ["", "Done."])

    SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution ->
      emit $
           renderTraversalSummary
             accepted
             nextNodes
             reachable
             rewardTotal
             score
             spent
             remaining
        ++ renderSolverSummary solution
        ++ ["", "Done."]