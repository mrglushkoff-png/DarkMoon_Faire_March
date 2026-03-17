{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_DMF_Project (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "DMF_Project"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Solver for DMF / Blingtron-style event graphs"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
