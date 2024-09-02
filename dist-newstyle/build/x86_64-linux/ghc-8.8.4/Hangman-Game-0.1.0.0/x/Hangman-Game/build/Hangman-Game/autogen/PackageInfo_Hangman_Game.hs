{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Hangman_Game (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Hangman_Game"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Jogo de forca com funcionalidade de torneio."
copyright :: String
copyright = "(c) 2024 Orlandovpjunior"
homepage :: String
homepage = ""
