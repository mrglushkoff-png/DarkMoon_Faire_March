module Config
  ( getConfig
  ) where

import Data.Char (toLower)
import Data.List (dropWhileEnd)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import EventLoader
import Types

prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  trim <$> getLine

trim :: String -> String
trim =
  dropWhileEnd (== ' ') . dropWhile (== ' ')

lower :: String -> String
lower = map toLower

promptUntil :: String -> (String -> Maybe a) -> IO a
promptUntil message parse = do
  input <- prompt message
  case parse input of
    Just value ->
      pure value
    Nothing -> do
      putStrLn "Invalid input, please try again."
      promptUntil message parse

promptWithDefault :: String -> a -> (String -> Maybe a) -> IO a
promptWithDefault message def parse = do
  input <- prompt message
  if null input
    then pure def
    else case parse input of
           Just value -> pure value
           Nothing -> do
             putStrLn "Invalid input, please try again."
             promptWithDefault message def parse

parseInt :: String -> Maybe Int
parseInt = readMaybe

parseDouble :: String -> Maybe Double
parseDouble = readMaybe

parseYesNo :: String -> Maybe Bool
parseYesNo input =
  case lower input of
    "y"   -> Just True
    "yes" -> Just True
    "n"   -> Just False
    "no"  -> Just False
    _     -> Nothing

parseInputMode :: String -> Maybe InputMode
parseInputMode input =
  case lower input of
    "1"   -> Just CliMode
    "cli" -> Just CliMode
    "2"   -> Just TuiMode
    "tui" -> Just TuiMode
    _     -> Nothing

getEventName :: IO EventName
getEventName = do
  putStrLn "Select event:"
  putStrLn "  1) DarkmoonFaire"
  promptUntil "Choice: " parseEventName

getInputMode :: IO InputMode
getInputMode = do
  putStrLn "\nSelect input mode:"
  putStrLn "  1) CLI"
  putStrLn "  2) TUI"
  promptUntil "Choice: " parseInputMode

getValuation :: IO Valuation
getValuation = do
  useDefault <- promptUntil "\nUse default valuation package? (y/n): " parseYesNo

  if useDefault
    then do
      putStrLn "Defaults:"
      putStrLn "  5000 EXP -> 100 G"
      putStrLn "  1 Troop -> 90 G"
      putStrLn "  1 Leader -> 120 G"
      putStrLn "  500 AE claimed from nodes -> 100 G"
      putStrLn "  1 Rare Core -> 250 G"
      putStrLn "  500 leftover tickets at event end -> 0 G"
      pure defaultValuation
    else do
      expGold <- promptWithDefault
        "How much gold do you value 5000 EXP at? (default 100): "
        100
        parseDouble

      troopGold <- promptWithDefault
        "How much gold do you value 1 Troop at? (default 90): "
        90
        parseDouble

      leaderGold <- promptWithDefault
        "How much gold do you value 1 Leader at? (default 120): "
        120
        parseDouble

      aeGold <- promptWithDefault
        "How much gold do you value 500 AE claimed from nodes at? (default 100): "
        100
        parseDouble

      rareGold <- promptWithDefault
        "How much gold do you value 1 Rare Core at? (default 250): "
        250
        parseDouble

      leftoverGold <- promptWithDefault
        "How much gold do you value 500 leftover tickets at event end at? (default 0): "
        0
        parseDouble

      pure $
        Valuation
          { valueExp            = expGold / 5000
          , valueMinorStar      = troopGold
          , valueMajorStar      = leaderGold
          , valueArclightEnergy = aeGold / 500
          , valueRareCore       = rareGold
          , valueLeftoverTicket = leftoverGold / 500
          }

getConfig :: IO Config
getConfig = do
  ev   <- getEventName
  mode <- getInputMode
  t    <- promptUntil "Available tickets right now: " parseInt
  v    <- getValuation
  dbg  <- promptUntil "\nEnable debug logs? (y/n): " parseYesNo

  pure $
    Config
      { eventName    = ev
      , inputMode    = mode
      , tickets      = t
      , valuation    = v
      , debugEnabled = dbg
      }