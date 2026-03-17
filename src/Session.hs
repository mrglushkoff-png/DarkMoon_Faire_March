module Session
  ( SessionResult(..)
  , runSessionM
  ) where

import Control.Monad.Reader (asks)

import App
import Graph
import Solver
import Types

data SessionResult
  = SessionInvalid ValidationError
  | SessionValid
      { srAccepted            :: [NodeId]
      , srNext                :: [NodeId]
      , srReachable           :: [NodeId]
      , srReward              :: Reward
      , srScore               :: Double
      , srSpent               :: Int
      , srRemaining           :: Int
      , srSolution            :: SearchResult
      , srLeftoverTicketValue :: Double
      , srTotalOutcomeScore   :: Double
      }
  deriving (Eq, Show)

runSessionM :: Graph -> [NodeId] -> App SessionResult
runSessionM graph claimed = do
  logWhenDebug Session ("claimed input = " ++ show claimed)

  validationResult <- validateClaimedM graph claimed

  case validationResult of
    Left err -> do
      logWhenDebug Session ("validation failed = " ++ show err)
      pure (SessionInvalid err)

    Right ok -> do
      currentValuation <- asks valuation

      let graph'      = markClaimed ok graph
          rewardTotal = claimedReward graph'
          spentTotal  = claimedCost graph'
          nextNodes   = claimableNow graph' ok
          reachable   = reachableIds graph' ok

      score     <- claimedScoreM graph'
      remaining <- remainingTicketsM graph'
      solution  <- solveRemainingM graph'

      let leftoverTicketValue = scoreLeftoverTickets currentValuation remaining
          totalOutcomeScore   = score + resultScore solution + leftoverTicketValue

      logBlockWhenDebug Session
        [ "accepted = " ++ show ok
        , "next = " ++ show nextNodes
        , "reachable = " ++ show reachable
        , "leftover ticket value = " ++ show leftoverTicketValue
        , "total outcome score = " ++ show totalOutcomeScore
        ]

      pure $
        SessionValid
          { srAccepted            = ok
          , srNext                = nextNodes
          , srReachable           = reachable
          , srReward              = rewardTotal
          , srScore               = score
          , srSpent               = spentTotal
          , srRemaining           = remaining
          , srSolution            = solution
          , srLeftoverTicketValue = leftoverTicketValue
          , srTotalOutcomeScore   = totalOutcomeScore
          }