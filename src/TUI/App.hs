module TUI.App
  ( runTui
  ) where

import Brick
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

import TUI.Draw
import TUI.Event
import TUI.Types
import Types

runTui :: Config -> Graph -> IO TuiState
runTui cfg graph = do
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty Nothing app (initialTuiState cfg graph)

app :: App TuiState e Name
app =
  App
    { appDraw         = drawUi
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = appEvent
    , appStartEvent   = pure ()
    , appAttrMap      = const tuiAttrMap
    }

tuiAttrMap :: AttrMap
tuiAttrMap =
  attrMap
    V.defAttr
    [ (attrName "claimed", fg V.green)
    , (attrName "claimable", fg V.yellow)
    , (attrName "reachable", fg V.cyan)
    , (attrName "blocked", fg V.brightBlack)

    , (attrName "cursorClaimed", fg V.green `V.withStyle` V.reverseVideo)
    , (attrName "cursorClaimable", fg V.yellow `V.withStyle` V.reverseVideo)
    , (attrName "cursorReachable", fg V.cyan `V.withStyle` V.reverseVideo)
    , (attrName "cursorBlocked", fg V.brightBlack `V.withStyle` V.reverseVideo)
    ]