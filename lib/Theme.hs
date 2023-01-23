{-# LANGUAGE TemplateHaskell #-}
module Theme(
    module Theme,
    module Colors
) where

import Colors
import Language.Haskell.TH
import TH.Theme

theme = $(conE (mkName $(retrieveThemeName) ))



