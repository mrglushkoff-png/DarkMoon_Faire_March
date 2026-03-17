module EventLoader
  ( availableEvents
  , parseEventName
  , loadEventGraphM
  ) where

import App
import Event.DarkmoonFaire
import Graph
import Types

availableEvents :: [EventName]
availableEvents = [minBound .. maxBound]

parseEventName :: String -> Maybe EventName
parseEventName s =
  case s of
    "1"              -> Just DarkmoonFaire
    "darkmoon"       -> Just DarkmoonFaire
    "darkmoonfaire"  -> Just DarkmoonFaire
    "DarkmoonFaire"  -> Just DarkmoonFaire
    _                -> Nothing

loadEventGraphM :: EventName -> App Graph
loadEventGraphM ev =
  case ev of
    DarkmoonFaire -> do
      logWhenDebug Session ("loading event = " ++ show ev)
      buildGraphM darkmoonPayloads