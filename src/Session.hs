module Session
  ( SessionResult(..)
  , runSessionM
  ) where

import App
import Graph
import Solver
import Types

data SessionResult
  = SessionInvalid ValidationError
  | SessionValid
      { srAccepted   :: [NodeId]
      , srNext       :: [NodeId]
      , srReachable  :: [NodeId]
      , srReward     :: Reward
      , srScore      :: Double
      , srSpent      :: Int
      , srRemaining  :: Int
      , srSolution   :: SearchResult
      }

runSessionM :: Graph -> [NodeId] -> App SessionResult
runSessionM graph claimed = do
  logWhenDebug Session ("claimed input = " ++ show claimed)

  validationResult <- validateClaimedM graph claimed

  case validationResult of
    Left err -> do
      logWhenDebug Session ("validation failed = " ++ show err)
      pure (SessionInvalid err)

    Right ok -> do
      let graph'      = markClaimed ok graph
          rewardTotal = claimedReward graph'
          spentTotal  = claimedCost graph'
          nextNodes   = claimableNow graph' ok
          reachable   = reachableIds graph' ok

      score     <- claimedScoreM graph'
      remaining <- remainingTicketsM graph'
      solution  <- solveRemainingM graph'

      logWhenDebug Session ("accepted = " ++ show ok)
      logWhenDebug Session ("next = " ++ show nextNodes)
      logWhenDebug Session ("reachable = " ++ show reachable)

      pure $
        SessionValid
          { srAccepted  = ok
          , srNext      = nextNodes
          , srReachable = reachable
          , srReward    = rewardTotal
          , srScore     = score
          , srSpent     = spentTotal
          , srRemaining = remaining
          , srSolution  = solution
          }