module TUI.Event
  ( appEvent
  ) where

import Brick
import qualified Graphics.Vty as V

import App
import Graph
import Session
import TUI.Types
import Types

appEvent :: BrickEvent Name e -> EventM Name TuiState ()
appEvent ev =
  case ev of
    VtyEvent (V.EvKey (V.KChar 'q') []) ->
      halt

    VtyEvent (V.EvKey (V.KChar ' ') []) -> do
      st <- get
      put (toggleClaim st)

    VtyEvent (V.EvKey V.KEnter []) -> do
      st <- get
      put (toggleClaim st)

    VtyEvent (V.EvKey (V.KChar 's') []) -> do
      st <- get
      put (runSolve st)

    VtyEvent (V.EvKey V.KLeft []) -> do
      st <- get
      put (moveCursor prevNodeId st)

    VtyEvent (V.EvKey V.KRight []) -> do
      st <- get
      put (moveCursor nextNodeId st)

    VtyEvent (V.EvKey V.KUp []) -> do
      st <- get
      put (moveCursor prevRowNodeId st)

    VtyEvent (V.EvKey V.KDown []) -> do
      st <- get
      put (moveCursor nextRowNodeId st)

    VtyEvent (V.EvKey V.KPageUp []) ->
      vScrollBy (viewportScroll InfoViewport) (-10)

    VtyEvent (V.EvKey V.KPageDown []) ->
      vScrollBy (viewportScroll InfoViewport) 10

    VtyEvent (V.EvKey (V.KChar 'k') []) ->
      vScrollBy (viewportScroll InfoViewport) (-3)

    VtyEvent (V.EvKey (V.KChar 'j') []) ->
      vScrollBy (viewportScroll InfoViewport) 3

    _ ->
      pure ()

toggleClaim :: TuiState -> TuiState
toggleClaim st =
  let nid = tuiCursor st
      claimed'
        | nid `elem` tuiClaimed st = filter (/= nid) (tuiClaimed st)
        | otherwise                = nid : tuiClaimed st
  in case validateClaimed (tuiGraph st) claimed' of
       Left err ->
         st
           { tuiStatus  = "Invalid: " ++ show err
           , tuiSession = Nothing
           , tuiLogs    = []
           }
       Right accepted ->
         st
           { tuiClaimed   = accepted
           , tuiStatus    = "Traversal valid"
           , tuiMode      = Editing
           , tuiNext      = claimableNow (tuiGraph st) accepted
           , tuiReachable = reachableIds (tuiGraph st) accepted
           , tuiSession   = Nothing
           , tuiLogs      = []
           }

runSolve :: TuiState -> TuiState
runSolve st =
  let graph' =
        markClaimed (tuiClaimed st) (tuiGraph st)

      (sessionResult, sessionLogs) =
        runApp (tuiConfig st) (runSessionM graph' (tuiClaimed st))

      statusText =
        case sessionResult of
          SessionInvalid err -> "Solve failed: " ++ show err
          SessionValid{}     -> "Solve complete"
  in st
       { tuiGraph    = graph'
       , tuiStatus   = statusText
       , tuiMode     = Solved
       , tuiSession  = Just sessionResult
       , tuiLogs     = sessionLogs
       }

moveCursor :: (Graph -> NodeId -> NodeId) -> TuiState -> TuiState
moveCursor step st =
  st { tuiCursor = step (tuiGraph st) (tuiCursor st) }

prevNodeId, nextNodeId, prevRowNodeId, nextRowNodeId :: Graph -> NodeId -> NodeId
prevNodeId graph current =
  case reverse (takeWhile (/= current) ids) of
    x:_ -> x
    []  -> current
  where
    ids = map nodeId graph

nextNodeId graph current =
  case dropWhile (/= current) (map nodeId graph) of
    _ : x : _ -> x
    _         -> current

prevRowNodeId graph current =
  maybe current nodeId $
    lookupNode current graph >>= \n ->
      firstInRow (row n - 1) graph

nextRowNodeId graph current =
  maybe current nodeId $
    lookupNode current graph >>= \n ->
      firstInRow (row n + 1) graph

firstInRow :: Row -> Graph -> Maybe Node
firstInRow target =
  go
  where
    go [] = Nothing
    go (n:ns)
      | row n == target = Just n
      | otherwise       = go ns