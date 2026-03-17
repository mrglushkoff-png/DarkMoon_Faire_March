module Config
  ( getConfig
  ) where

import EventLoader
import Types

readInt :: String -> IO Int
readInt prompt = putStr prompt >> readLn

readDouble :: String -> IO Double
readDouble prompt = putStr prompt >> readLn

yesInput :: String -> Bool
yesInput s =
  s `elem` ["y", "Y", "yes", "YES", "Yes"]

getEventName :: IO EventName
getEventName = do
  putStrLn "Select event:"
  putStrLn "  1) DarkmoonFaire"
  putStr "Choice: "
  choice <- getLine
  case parseEventName choice of
    Just ev -> pure ev
    Nothing -> do
      putStrLn "Invalid event selection, defaulting to DarkmoonFaire."
      pure DarkmoonFaire

getValuation :: IO Valuation
getValuation = do
  putStrLn "\nUse default valuation? (y/n)"
  useDefault <- getLine

  if yesInput useDefault
    then pure defaultValuation
    else do
      expV   <- readDouble "Gold value per 1 EXP: "
      minorV <- readDouble "Minor star value: "
      majorV <- readDouble "Major star value: "
      aeV    <- readDouble "Gold value per 1 Arclight Energy: "
      rareV  <- readDouble "Gold value per 1 Rare core: "
      pure $
        Valuation
          { valueExp            = expV
          , valueMinorStar      = minorV
          , valueMajorStar      = majorV
          , valueArclightEnergy = aeV
          , valueRareCore       = rareV
          }

getConfig :: IO Config
getConfig = do
  ev <- getEventName
  t  <- readInt "Available tickets: "
  v  <- getValuation

  putStrLn "\nEnable debug logs? (y/n)"
  dbg <- yesInput <$> getLine

  pure $
    Config
      { eventName    = ev
      , tickets      = t
      , valuation    = v
      , debugEnabled = dbg
      }