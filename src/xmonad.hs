{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Core.MyEventHook
import Core.MyKeyBindings
import Core.MyLayoutHook
import Core.MyLogHook
import Core.MyMangeHook
import Core.MyPrograms
import Core.MyStartupHook
import Core.MyStatusBar
import Color.Theme
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Util.NamedActions

-----------------------------------------------------------------

-- main
main :: IO ()
main = xmonad . withSB mySB . docks . ewmhFullscreen . ewmh $ addDescrKeys' ((mod4Mask, xK_F1), xMessage) myKeys defaults

-----------------------------------------------------------------
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
defaults =
  def -- simple stuff
    { terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      mouseBindings = myMouseBindings,
      layoutHook = myLayout,
      manageHook = myManageHook,
      handleEventHook = myEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

-----------------------------------------------------------------------
------------------------------------------------------------------------
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- myWorkspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = ["home", "fecu", "www", "docs", "dev", "sys-mon"] -- map show [1..9::Int]

myNormalBorderColor :: String
myNormalBorderColor = colorBlue theme

myFocusedBorderColor :: String
myFocusedBorderColor = colorOrange theme
