{-# LANGUAGE TemplateHaskell #-}
module TH.Theme where

import Language.Haskell.TH
import System.IO
import Language.Haskell.TH.Syntax


retrieveName =  
  do
    handle <- openFile "theme.txt" ReadMode
    name <-hGetLine handle
    return $ LitE (StringL name )

    
