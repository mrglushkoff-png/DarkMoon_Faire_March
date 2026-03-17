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
    "1"    -> Just CliMode
    "cli"  -> Just CliMode
    "2"    -> Just TuiMode
    "tui"  -> Just TuiMode
    _      -> Nothing

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
  useDefault <- promptUntil "\nUse default valuation? (y/n): " parseYesNo

  if useDefault
    then pure defaultValuation
    else do
      expV   <- promptUntil "Gold value per 1 EXP: " parseDouble
      minorV <- promptUntil "Minor star value: " parseDouble
      majorV <- promptUntil "Major star value: " parseDouble
      aeV    <- promptUntil "Gold value per 1 Arclight Energy: " parseDouble
      rareV  <- promptUntil "Gold value per 1 Rare core: " parseDouble

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
  ev   <- getEventName
  mode <- getInputMode
  t    <- promptUntil "Available tickets: " parseInt
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