module TUI.Draw
  ( drawUi
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Render
import Session
import TUI.Types
import Types

drawUi :: TuiState -> [Widget Name]
drawUi st =
  [ center $
      hBox
        [ borderWithLabel (str "Graph") (padAll 1 (drawGraph st))
        , borderWithLabel (str "Info")  (padAll 1 (hLimit 60 (drawInfo st)))
        ]
  ]

drawGraph :: TuiState -> Widget Name
drawGraph st =
  vBox (map drawRow (groupRows (tuiGraph st)))
  where
    drawRow =
      hBox . map (padRight (Pad 1) . drawNode st)

drawNode :: TuiState -> Node -> Widget Name
drawNode st node =
  let nid      = nodeId node
      here     = nid == tuiCursor st
      claimed  = nid `elem` tuiClaimed st
      nextNow  = nid `elem` tuiNext st
      reach    = nid `elem` tuiReachable st
      marker
        | claimed   = "[*]"
        | otherwise = "[ ]"
      body = marker ++ nid
  in withAttr (nodeAttr here claimed nextNow reach) (str body)

nodeAttr :: Bool -> Bool -> Bool -> Bool -> AttrName
nodeAttr here claimed nextNow reach =
  case (here, claimed, nextNow, reach) of
    (True,  True,  _,     _)     -> attrName "cursorClaimed"
    (True,  False, True,  _)     -> attrName "cursorClaimable"
    (True,  False, False, True)  -> attrName "cursorReachable"
    (True,  False, False, False) -> attrName "cursorBlocked"
    (False, True,  _,     _)     -> attrName "claimed"
    (False, False, True,  _)     -> attrName "claimable"
    (False, False, False, True)  -> attrName "reachable"
    (False, False, False, False) -> attrName "blocked"

drawInfo :: TuiState -> Widget Name
drawInfo st =
  vBox (map (padBottom (Pad 0) . str) infoLines)
  where
    infoLines =
         [ "Cursor: " ++ tuiCursor st
         , "Mode: " ++ show (tuiMode st)
         , ""
         , "Legend:"
         , "  [*] claimed"
         , "  [ ] unclaimed"
         , "  green   = claimed"
         , "  yellow  = claimable now"
         , "  blue    = reachable later"
         , "  dim     = blocked"
         , ""
         , "Claimed: " ++ unwords (tuiClaimed st)
         , ""
         , "Currently claimable:"
         , unwords (tuiNext st)
         , ""
         , "Remaining reachable:"
         , unwords (tuiReachable st)
         , ""
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

groupRows :: Graph -> [[Node]]
groupRows [] = []
groupRows (n:ns) =
  let currentRow = row n
      (sameRow, rest) = span ((== currentRow) . row) ns
  in (n : sameRow) : groupRows rest