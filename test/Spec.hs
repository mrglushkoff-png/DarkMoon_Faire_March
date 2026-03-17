module Main where

import Test.Hspec

import qualified GraphSpec
import qualified RenderSpec
import qualified SessionSpec
import qualified SolverSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
  TypesSpec.spec
  GraphSpec.spec
  RenderSpec.spec
  SolverSpec.spec
  SessionSpec.spec