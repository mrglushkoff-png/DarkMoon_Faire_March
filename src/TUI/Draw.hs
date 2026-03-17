module TUI.Draw
  ( drawUi
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Data.Text as T

import Render
import Session
import TUI.Types
import Types

drawUi :: TuiState -> [Widget Name]
drawUi st =
  [ center $
      hBox
        [ borderWithLabel (str "Graph") (padAll 1 (drawGraph st))
        , padLeft (Pad 1) $
            borderWithLabel (str "Info") $
              hLimitPercent 60 $
                viewport InfoViewport Vertical $
                  padAll 1 (drawInfo st)
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
  vBox (map renderLine infoLines)
  where
    renderLine :: String -> Widget Name
    renderLine = padBottom (Pad 0) . txtWrap . T.pack

    infoLines =
         [ "Controls:"
         , "  Arrows  = move cursor"
         , "  Space   = toggle claimed"
         , "  Enter   = toggle claimed"
         , "  s       = solve"
         , "  q       = quit"
         , "  PgUp/PgDn = scroll info"
         , "  j / k   = scroll info"
         , ""
         , "Cursor: " ++ tuiCursor st
         , "Mode: " ++ show (tuiMode st)
         ]
      ++ drawCursorDetails st
      ++ [ ""
         , "Legend:"
         , "  [*] claimed"
         , "  [ ] unclaimed"
         , "  green   = claimed"
         , "  yellow  = claimable now"
         , "  cyan    = reachable later"
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

drawCursorDetails :: TuiState -> [String]
drawCursorDetails st =
  case lookupCursorNode st of
    Nothing ->
      []

    Just node ->
      [ ""
      , "Hovered node:"
      , "  Node: " ++ nodeId node
      , "  Cost: " ++ show (cost node)
      , "  Reward: " ++ renderRewardCompact (reward node)
      , "  Parents: " ++ renderParents (parents node)
      ]

lookupCursorNode :: TuiState -> Maybe Node
lookupCursorNode st =
  go (tuiGraph st)
  where
    target = tuiCursor st

    go [] = Nothing
    go (n:ns)
      | nodeId n == target = Just n
      | otherwise          = go ns

drawSessionLines :: TuiState -> [String]
drawSessionLines st =
  case tuiSession st of
    Nothing ->
      []

    Just (SessionInvalid err) ->
      renderValidationFailure err

    Just (SessionValid accepted nextNodes reachable rewardTotal score spent remaining solution leftoverTicketValue totalOutcomeScore) ->
      renderTraversalSummary
        accepted
        nextNodes
        reachable
        rewardTotal
        score
        spent
        remaining
        leftoverTicketValue
        totalOutcomeScore
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