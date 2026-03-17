module App
  ( App
  , runApp
  , logMsg
  , logWhenDebug
  , logNamed
  , logBlockWhenDebug
  ) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict

import Types

type App = ReaderT Config (Writer [LogEntry])

runApp :: Config -> App a -> (a, [LogEntry])
runApp cfg = runWriter . flip runReaderT cfg

logMsg :: LogPhase -> String -> App ()
logMsg phase msg =
  tell [LogEntry phase msg]

logWhenDebug :: LogPhase -> String -> App ()
logWhenDebug phase msg = do
  dbg <- asks debugEnabled
  if dbg then logMsg phase msg else pure ()

logNamed :: Show a => LogPhase -> String -> a -> App ()
logNamed phase name value =
  logWhenDebug phase (name ++ " = " ++ show value)

logBlockWhenDebug :: LogPhase -> [String] -> App ()
logBlockWhenDebug phase =
  mapM_ (logWhenDebug phase)