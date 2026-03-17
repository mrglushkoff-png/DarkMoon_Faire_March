module Graph
  ( rowSizes
  , buildGraphM
  , graphRows
  , nodeIds
  , claimedIds
  , claimedNodes
  , claimedReward
  , claimedScoreM
  , claimedCost
  , remainingTicketsM
  , lookupNode
  , markClaimed
  , claimable
  , claimableNow
  , reachableIds
  , remainingReachableNodes
  , validateClaimed
  , validateClaimedM
  ) where

import Control.Monad.Reader (asks)
import Data.List (find, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import App
import Types

rowSizes :: [Int]
rowSizes = [4,3,4,3,4,3,4,3]

rowIds :: [[NodeId]]
rowIds =
  zipWith mkRow ['A' ..] rowSizes
  where
    mkRow label width =
      [ label : show i | i <- [1 .. width] ]

parentSpecs :: [(NodeId, [NodeId])]
parentSpecs =
  concat (zipWith connect rowIds (tail rowIds))
  where
    connect upper lower =
      case (upper, lower) of
        ([a1,a2,a3,a4], [b1,b2,b3]) ->
          [ (b1, [a1,a2])
          , (b2, [a2,a3])
          , (b3, [a3,a4])
          ]

        ([b1,b2,b3], [c1,c2,c3,c4]) ->
          [ (c1, [b1])
          , (c2, [b1,b2])
          , (c3, [b2,b3])
          , (c4, [b3])
          ]

        (xs, ys) ->
          error ("Unsupported row transition: " ++ show (length xs, length ys))

parentMap :: M.Map NodeId [NodeId]
parentMap =
  M.fromList parentSpecs

buildGraph :: [NodePayload] -> Graph
buildGraph payloads
  | length payloads /= expectedNodeCount =
      error "Payload count does not match graph size."
  | otherwise =
      zipWith attachPayload skeleton payloads
  where
    expectedNodeCount = sum rowSizes

    skeleton =
      [ (nid, r, M.findWithDefault [] nid parentMap)
      | (r, idsInRow) <- zip [1 ..] rowIds
      , nid <- idsInRow
      ]

    attachPayload (nid, r, ps) payload =
      Node
        { nodeId    = nid
        , row       = r
        , cost      = payloadCost payload
        , reward    = payloadReward payload
        , parents   = ps
        , isClaimed = payloadClaimed payload
        }

buildGraphM :: [NodePayload] -> App Graph
buildGraphM payloads = do
  logNamed GraphBuild "rowSizes" rowSizes
  logNamed GraphBuild "rowIds" rowIds
  logNamed GraphBuild "payload count" (length payloads)
  logNamed GraphBuild "expected node count" (sum rowSizes)
  logNamed GraphBuild "parent spec count" (length parentSpecs)

  let graph = buildGraph payloads

  logNamed GraphBuild "built graph node count" (length graph)
  logNamed GraphBuild "claimed-by-default" (claimedIds graph)

  pure graph

graphRows :: Graph -> [[Node]]
graphRows = groupRows . sortOn row
  where
    groupRows [] = []
    groupRows (n:ns) =
      let currentRow = row n
          (sameRow, rest) = span ((== currentRow) . row) ns
      in (n : sameRow) : groupRows rest

nodeIds :: Graph -> [NodeId]
nodeIds = map nodeId

claimedIds :: Graph -> [NodeId]
claimedIds = map nodeId . filter isClaimed

claimedNodes :: Graph -> [Node]
claimedNodes = filter isClaimed

claimedReward :: Graph -> Reward
claimedReward =
  foldl' addReward zeroReward . map reward . claimedNodes

claimedScoreM :: Graph -> App Double
claimedScoreM graph = do
  let aggregateReward = claimedReward graph
  currentValuation <- asks valuation
  let totalScore = scoreReward currentValuation aggregateReward

  logNamed Scoring "claimed reward aggregate" aggregateReward
  logNamed Scoring "claimed reward score" totalScore

  pure totalScore

claimedCost :: Graph -> Int
claimedCost = sum . map cost . claimedNodes

remainingTicketsM :: Graph -> App Int
remainingTicketsM graph = do
  let spentTickets = claimedCost graph
  totalTickets <- asks tickets
  let remaining = totalTickets - spentTickets

  logBlockWhenDebug Ticketing
    [ "claimed cost = " ++ show spentTickets
    , "available tickets = " ++ show totalTickets
    , "remaining tickets = " ++ show remaining
    ]

  pure remaining

lookupNode :: NodeId -> Graph -> Maybe Node
lookupNode nid = find ((== nid) . nodeId)

markClaimed :: [NodeId] -> Graph -> Graph
markClaimed acceptedIds =
  map markNode
  where
    acceptedSet = S.fromList acceptedIds

    markNode node
      | nodeId node `S.member` acceptedSet = node { isClaimed = True }
      | otherwise                          = node

claimable :: S.Set NodeId -> Node -> Bool
claimable claimedSet node =
     row node == 1
  || any (`S.member` claimedSet) (parents node)

claimableNow :: Graph -> [NodeId] -> [NodeId]
claimableNow graph acceptedIds =
  [ nodeId node
  | node <- graph
  , nodeId node `S.notMember` acceptedSet
  , claimable acceptedSet node
  ]
  where
    acceptedSet = S.fromList acceptedIds

reachableIds :: Graph -> [NodeId] -> [NodeId]
reachableIds graph acceptedIds =
  orderedReachableIds
  where
    acceptedSet = S.fromList acceptedIds

    unclaimedSet =
      S.fromList
        [ nodeId node
        | node <- graph
        , nodeId node `S.notMember` acceptedSet
        ]

    expand reachableSet =
      reachableSet <>
      S.fromList
        [ nodeId node
        | node <- graph
        , nodeId node `S.member` unclaimedSet
        , nodeId node `S.notMember` reachableSet
        , isReachable reachableSet node
        ]

    isReachable reachableSet node =
      let unlockedSet = acceptedSet <> reachableSet
      in row node == 1 || any (`S.member` unlockedSet) (parents node)

    saturate reachableSet =
      let nextReachable = expand reachableSet
      in if nextReachable == reachableSet
           then reachableSet
           else saturate nextReachable

    finalReachableSet = saturate S.empty

    orderedReachableIds =
      [ nodeId node
      | node <- graph
      , nodeId node `S.member` finalReachableSet
      ]

remainingReachableNodes :: Graph -> [NodeId] -> [Node]
remainingReachableNodes graph acceptedIds =
  let reachableSet = S.fromList (reachableIds graph acceptedIds)
  in [ node
     | node <- graph
     , nodeId node `S.member` reachableSet
     ]

validateClaimed :: Graph -> [NodeId] -> Either ValidationError [NodeId]
validateClaimed graph inputIds =
  result
  where
    nodesById =
      M.fromList [ (nodeId node, node) | node <- graph ]

    wantedSet =
      S.fromList inputIds

    acceptedSet =
      saturate nodesById wantedSet S.empty

    acceptedInGraphOrder =
      [ nodeId node
      | node <- graph
      , nodeId node `S.member` acceptedSet
      ]

    duplicateId =
      firstDuplicate S.empty inputIds

    unknownId =
      find (`M.notMember` nodesById) inputIds

    disconnectedId =
      find (`S.notMember` acceptedSet) inputIds

    result
      | Just dup <- duplicateId    = Left (DuplicateNodeId dup)
      | Just bad <- unknownId      = Left (UnknownNodeId bad)
      | Just bad <- disconnectedId = Left (DisconnectedClaim bad)
      | otherwise                  = Right acceptedInGraphOrder

    firstDuplicate _    []     = Nothing
    firstDuplicate seen (x:xs)
      | x `S.member` seen = Just x
      | otherwise         = firstDuplicate (S.insert x seen) xs

    saturate nodesById wantedSet acceptedSet =
      let nextAccepted =
            S.fromList
              [ nid
              | nid <- S.toList (wantedSet S.\\ acceptedSet)
              , maybe False (claimable acceptedSet) (M.lookup nid nodesById)
              ]
      in if S.null nextAccepted
           then acceptedSet
           else saturate nodesById wantedSet (acceptedSet <> nextAccepted)

validateClaimedM :: Graph -> [NodeId] -> App (Either ValidationError [NodeId])
validateClaimedM graph inputIds = do
  let result = validateClaimed graph inputIds
      acceptedInGraphOrder =
        case result of
          Left _       -> []
          Right accepted -> accepted

  logBlockWhenDebug Validation
    [ "validation input = " ++ show inputIds
    , "known node ids = " ++ show (nodeIds graph)
    , "claimable from empty = " ++ show (claimableNow graph [])
    , "validation result = " ++ show result
    , "claimable after accepted = " ++ show (claimableNow graph acceptedInGraphOrder)
    , "reachable after accepted = " ++ show (reachableIds graph acceptedInGraphOrder)
    ]

  pure result