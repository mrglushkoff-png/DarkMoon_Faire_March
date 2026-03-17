module Solver
  ( SearchResult(..)
  , emptyResult
  , solveRemainingM
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.State.Strict
import Data.Bits
import Data.List (maximumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Word (Word64)

import App
import Graph
import Types

data SearchResult = SearchResult
  { resultPath   :: ![NodeId]
  , resultReward :: !Reward
  , resultCost   :: !Int
  , resultScore  :: !Double
  } deriving (Eq, Show)

emptyResult :: SearchResult
emptyResult =
  SearchResult
    { resultPath   = []
    , resultReward = zeroReward
    , resultCost   = 0
    , resultScore  = 0
    }

type ClaimMask = Word64
type SearchKey = (ClaimMask, Int)

data SearchState = SearchState
  { stateClaimedMask :: !ClaimMask
  , stateBudgetLeft  :: !Int
  } deriving (Eq, Ord, Show)

data MemoState = MemoState
  { memoCache  :: !(M.Map SearchKey SearchResult)
  , memoHits   :: !Int
  , memoMisses :: !Int
  } deriving (Eq, Show)

emptyMemoState :: MemoState
emptyMemoState =
  MemoState
    { memoCache  = M.empty
    , memoHits   = 0
    , memoMisses = 0
    }

type SearchM = StateT MemoState App

data IndexedNode = IndexedNode
  { indexedNode       :: !Node
  , indexedBit        :: !Int
  , indexedParentMask :: !ClaimMask
  }

data IndexedGraph = IndexedGraph
  { bitIndex      :: !(M.Map NodeId Int)
  , indexedById   :: !(M.Map NodeId IndexedNode)
  }

solveRemainingM :: Graph -> App SearchResult
solveRemainingM graph = do
  totalTickets     <- asks tickets
  currentValuation <- asks valuation

  let claimedNow     = claimedIds graph
      spentAlready   = claimedCost graph
      budgetLeft     = totalTickets - spentAlready
      candidateNodes = remainingReachableNodes graph claimedNow
      indexedGraph   = indexGraph graph
      initialMask    = claimedMaskOf indexedGraph claimedNow
      initialState   =
        SearchState
          { stateClaimedMask = initialMask
          , stateBudgetLeft  = budgetLeft
          }

  (bestResult, memoState) <-
    runStateT
      (solveState currentValuation indexedGraph candidateNodes initialState)
      emptyMemoState

  logBlockWhenDebug Solving
    [ "solver claimed ids = " ++ show claimedNow
    , "solver spent already = " ++ show spentAlready
    , "solver budget left = " ++ show budgetLeft
    , "solver universe = " ++ show (map nodeId candidateNodes)
    , "solver memo hits = " ++ show (memoHits memoState)
    , "solver memo misses = " ++ show (memoMisses memoState)
    , "solver cached states = " ++ show (M.size (memoCache memoState))
    , "solver best path = " ++ show (resultPath bestResult)
    , "solver best reward = " ++ show (resultReward bestResult)
    , "solver best cost = " ++ show (resultCost bestResult)
    , "solver best score = " ++ show (resultScore bestResult)
    ]

  pure bestResult

solveState :: Valuation -> IndexedGraph -> [Node] -> SearchState -> SearchM SearchResult
solveState valuation' indexedGraph candidateNodes searchState = do
  let key = (stateClaimedMask searchState, stateBudgetLeft searchState)

  cached <- gets (M.lookup key . memoCache)
  case cached of
    Just result -> do
      modify (\s -> s { memoHits = memoHits s + 1 })
      pure result

    Nothing -> do
      modify (\s -> s { memoMisses = memoMisses s + 1 })

      candidateResults <-
        mapM
          (extendWith valuation' indexedGraph candidateNodes searchState)
          (nextChoices indexedGraph candidateNodes searchState)

      let bestResult = bestOf (emptyResult : candidateResults)

      modify (\s -> s { memoCache = M.insert key bestResult (memoCache s) })

      pure bestResult

nextChoices :: IndexedGraph -> [Node] -> SearchState -> [Node]
nextChoices indexedGraph candidateNodes searchState =
  filter (isAvailable indexedGraph searchState) candidateNodes

isAvailable :: IndexedGraph -> SearchState -> Node -> Bool
isAvailable indexedGraph searchState node =
     not (isClaimedIn indexedGraph (stateClaimedMask searchState) (nodeId node))
  && cost node <= stateBudgetLeft searchState
  && claimableIn indexedGraph (stateClaimedMask searchState) node

extendWith
  :: Valuation
  -> IndexedGraph
  -> [Node]
  -> SearchState
  -> Node
  -> SearchM SearchResult
extendWith valuation' indexedGraph candidateNodes searchState node = do
  let nextState =
        SearchState
          { stateClaimedMask = claimNode indexedGraph (stateClaimedMask searchState) (nodeId node)
          , stateBudgetLeft  = stateBudgetLeft searchState - cost node
          }

  subResult <- solveState valuation' indexedGraph candidateNodes nextState

  let combinedReward = addReward (reward node) (resultReward subResult)
      combinedCost   = cost node + resultCost subResult
      combinedPath   = nodeId node : resultPath subResult
      combinedScore  = scoreReward valuation' combinedReward

  pure
    SearchResult
      { resultPath   = combinedPath
      , resultReward = combinedReward
      , resultCost   = combinedCost
      , resultScore  = combinedScore
      }

bestOf :: [SearchResult] -> SearchResult
bestOf =
  maximumBy
    (  comparing resultScore
    <> flip (comparing resultCost)
    <> comparing (length . resultPath)
    )

indexGraph :: Graph -> IndexedGraph
indexGraph graph
  | length graph > 64 =
      error "Bitset solver supports at most 64 nodes."
  | otherwise =
      IndexedGraph
        { bitIndex    = bitMap
        , indexedById = indexedMap
        }
  where
    bitMap =
      M.fromList (zip (map nodeId graph) [0 ..])

    indexedMap =
      M.fromList
        [ (nodeId node, toIndexed node)
        | node <- graph
        ]

    toIndexed node =
      IndexedNode
        { indexedNode       = node
        , indexedBit        = lookupBit (nodeId node)
        , indexedParentMask = foldl setParentBit 0 (parents node)
        }

    lookupBit nid =
      case M.lookup nid bitMap of
        Just bitIx -> bitIx
        Nothing    -> error ("Missing bit index for node " ++ nid)

    setParentBit mask nid =
      setBit mask (lookupBit nid)

claimedMaskOf :: IndexedGraph -> [NodeId] -> ClaimMask
claimedMaskOf indexedGraph =
  foldl setClaimBit 0
  where
    setClaimBit mask nid =
      case M.lookup nid (bitIndex indexedGraph) of
        Just bitIx -> setBit mask bitIx
        Nothing    -> mask

claimNode :: IndexedGraph -> ClaimMask -> NodeId -> ClaimMask
claimNode indexedGraph mask nid =
  case M.lookup nid (bitIndex indexedGraph) of
    Just bitIx -> setBit mask bitIx
    Nothing    -> mask

isClaimedIn :: IndexedGraph -> ClaimMask -> NodeId -> Bool
isClaimedIn indexedGraph mask nid =
  case M.lookup nid (bitIndex indexedGraph) of
    Just bitIx -> testBit mask bitIx
    Nothing    -> False

claimableIn :: IndexedGraph -> ClaimMask -> Node -> Bool
claimableIn indexedGraph claimedMask node =
  case M.lookup (nodeId node) (indexedById indexedGraph) of
    Just iNode ->
         row node == 1
      || (indexedParentMask iNode .&. claimedMask) /= 0
    Nothing ->
      False