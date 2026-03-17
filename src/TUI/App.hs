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
    [ (attrName "normal", V.defAttr)
    , (attrName "claimed", fg V.green)
    , (attrName "cursor", V.defAttr `V.withStyle` V.reverseVideo)
    , (attrName "cursorClaimed", fg V.green `V.withStyle` V.reverseVideo)
    ]