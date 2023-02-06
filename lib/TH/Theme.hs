module TH.Theme where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO

retrieveThemeName :: Q Exp
retrieveThemeName = do
  addDependentFile "/home/karim/.local/theme.txt"
  runIO $ do
    name <- withFile "/home/karim/.local/theme.txt" ReadMode hGetLine
    return $ LitE (StringL name)
