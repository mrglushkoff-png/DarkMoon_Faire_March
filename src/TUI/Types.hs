module TUI.Types
  ( Name(..)
  , Mode(..)
  , TuiState(..)
  , initialTuiState
  ) where

import Graph (claimedIds, claimableNow, reachableIds)
import Session
import Types

data Name
  = GraphViewport
  deriving (Eq, Ord, Show)

data Mode
  = Editing
  | Solved
  deriving (Eq, Show)

data TuiState = TuiState
  { tuiConfig     :: !Config
  , tuiGraph      :: !Graph
  , tuiCursor     :: !NodeId
  , tuiClaimed    :: ![NodeId]
  , tuiStatus     :: !String
  , tuiMode       :: !Mode
  , tuiNext       :: ![NodeId]
  , tuiReachable  :: ![NodeId]
  , tuiSession    :: !(Maybe SessionResult)
  , tuiLogs       :: ![LogEntry]
  } deriving (Eq, Show)

initialTuiState :: Config -> Graph -> TuiState
initialTuiState cfg graph =
  let claimed = claimedIds graph
  in TuiState
       { tuiConfig    = cfg
       , tuiGraph     = graph
       , tuiCursor    =
           case graph of
             n:_ -> nodeId n
             []  -> ""
       , tuiClaimed   = claimed
       , tuiStatus    = "Arrow keys move, space toggles, s solves, q quits"
       , tuiMode      = Editing
       , tuiNext      = claimableNow graph claimed
       , tuiReachable = reachableIds graph claimed
       , tuiSession   = Nothing
       , tuiLogs      = []
       }