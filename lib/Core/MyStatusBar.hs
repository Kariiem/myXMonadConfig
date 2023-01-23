module Core.MyStatusBar where

import Color.Theme
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet qualified as W
import XMonad.Util.ClickableWorkspaces

myPP :: PP
myPP =
  def
    { ppCurrent = xmobarColor (colorRed theme) "",
      ppUrgent = xmobarColor (colorGreen theme) (colorBPurple theme),
      ppLayout = xmobarFont 5 . xmobarColor (colorBPurple theme) "" . showLayoutName,
      ppSep = " ",
      ppWsSep = " ",
      ppExtras = [windowCount],
      ppTitle = xmobarColor (colorFore theme) "" . shorten 45,
      ppHidden = xmobarColor (colorCyan theme) "",
      ppHiddenNoWindows = xmobarColor (colorGrey theme) "",
      ppOrder = \(ws : l : t : ex) -> ws : ex ++ l : [t]
    }

windowCount :: X (Maybe String)
windowCount =
  Just
    . xmobarColor (colorGreen theme) ""
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset
    <$> get

mySB :: StatusBarConfig
mySB =
  statusBarProp "xmobar" $
    copiesPP (xmobarFont 4 . xmobarColor (colorBlack theme) "") myPP >>= clickablePP

-- xmobarConfigPath
-- xmobarConfigPath (pure sjanssenPP)
-- mySB = statusBarProp xmobarConfigPath (pure myPP)
showLayoutName :: String -> String
showLayoutName x = pad $
  case x of
    "Spacing ResizableThreeCol" -> "[─┤ ├─]"
    "Mirror Spacing ResizableThreeCol" -> "[─┴──┬─]"
    "Spacing ResizableTall" -> "[ ├─]"
    "Mirror Spacing ResizableTall" -> "[─┬─]"
    "Spacing Grid" -> "[#]"
    "Full" -> "[ ]"
    _ -> x
