{-# LANGUAGE TemplateHaskell #-}
module Theme(
    module Theme,
    module Colors
) where

import Colors
import Language.Haskell.TH
import TH.Theme
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Syntax

theme = $(conE (mkName $(runIO retrieveName )))



