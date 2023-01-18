module TH.Theme where

import Language.Haskell.TH
import System.IO
import Language.Haskell.TH.Syntax


retrieveThemeName :: Q Exp
retrieveThemeName = do
  addDependentFile "theme.txt"
  runIO $ do
    handle <- openFile "theme.txt" ReadMode
    name<-hGetLine handle
    return $ LitE (StringL name )

