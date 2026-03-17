module TUI.Draw
  ( drawUi
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Graph
import Render
import Session
import Solver
import TUI.Types
import Types

drawUi :: TuiState -> [Widget Name]
drawUi st =
  [ center $
      hBox
        [ borderWithLabel (str "Graph") (padAll 1 (drawGraph st))
        , borderWithLabel (str "Info")  (padAll 1 (hLimit 56 (drawInfo st)))
        ]
  ]

drawGraph :: TuiState -> Widget Name
drawGraph st =
  vBox (map drawRow (graphRows (tuiGraph st)))
  where
    drawRow =
      hBox . map (padRight (Pad 1) . drawNode st)

drawNode :: TuiState -> Node -> Widget Name
drawNode st node =
  let nid     = nodeId node
      here    = nid == tuiCursor st
      claimed = nid `elem` tuiClaimed st
      marker
        | claimed   = "[*]"
        | otherwise = "[ ]"
      body = marker ++ nid
  in withAttr (nodeAttr here claimed) (str body)

nodeAttr :: Bool -> Bool -> AttrName
nodeAttr here claimed =
  case (here, claimed) of
    (True,  True)  -> attrName "cursorClaimed"
    (True,  False) -> attrName "cursor"
    (False, True)  -> attrName "claimed"
    (False, False) -> attrName "normal"

drawInfo :: TuiState -> Widget Name
drawInfo st =
  vBox (map (padBottom (Pad 0) . str) infoLines)
  where
    infoLines =
         [ "Cursor: " ++ tuiCursor st
         , "Mode: " ++ show (tuiMode st)
         , "Claimed: " ++ unwords (tuiClaimed st)
         , "Currently claimable: " ++ unwords (tuiNext st)
         , "Remaining reachable: " ++ unwords (tuiReachable st)
         , "Status: " ++ tuiStatus st
         ]
      ++ drawSessionLines st
      ++ drawLogLines st

drawSessionLines :: TuiState -> [String]
drawSessionLines st =
  case tuiSession st of
    Nothing ->
      []

    Just (SessionInvalid err) ->
      renderValidationFailure err

    Just (SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution) ->
      renderTraversalSummary
        accepted
        nextNodes
        reachable
        rewardTotal
        score
        spent
        remaining
      ++ renderSolverSummary solution

drawLogLines :: TuiState -> [String]
drawLogLines st =
  renderLogs (tuiLogs st)