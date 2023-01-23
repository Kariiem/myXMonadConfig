{-# LANGUAGE TemplateHaskell #-}

module Color.Theme
  ( module Color.Theme,
    module Color.MyPalette,
  )
where

import Color.MyPalette
import Language.Haskell.TH
import TH.Theme

theme = $(conE (mkName $(retrieveThemeName)))
