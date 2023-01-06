module Bar where

import qualified XMonad.StackSet as W
import           XMonad
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.StatusBar
import 		 XMonad.Util.ClickableWorkspaces
import 		 XMonad.Actions.CopyWindow

myPP :: PP
myPP = def { ppCurrent = xmobarColor "purple" ""
           , ppUrgent = xmobarColor "green" "pink"
           , ppLayout = xmobarFont 5 . xmobarColor "white" "" . showLayoutName
           , ppSep = " "
           , ppWsSep = " "
           , ppExtras = [windowCount]
           , ppTitle = xmobarColor "white" "" . shorten 45
           , ppHidden = xmobarColor "#EEEEEE" ""
           , ppHiddenNoWindows = xmobarColor "grey" ""
           , ppOrder = \(ws:l:t:ex) -> ws:ex ++ l:[t]
           }

windowCount :: X (Maybe String)
windowCount = Just . xmobarColor "green" ""
  . show
  . length
  . W.integrate'
  . W.stack
  . W.workspace
  . W.current
  . windowset
  <$> get

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar"
  $ copiesPP (xmobarFont 4 . xmobarColor "black" "") myPP >>= clickablePP

  -- xmobarConfigPath
  --xmobarConfigPath (pure sjanssenPP) 
-- mySB = statusBarProp xmobarConfigPath (pure myPP)
showLayoutName :: String -> String
showLayoutName x = pad
  $ case x of
    "Spacing ResizableThreeCol" -> "[─┤ ├─]"
    "Mirror Spacing ResizableThreeCol" -> "[─┴──┬─]"
    "Spacing ResizableTall" -> "[ ├─]"
    "Mirror Spacing ResizableTall" -> "[─┬─]"
    "Spacing Grid" -> "[#]"
    "Full" -> "[ ]"
    _ -> x

