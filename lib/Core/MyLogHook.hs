module Core.MyLogHook where

import XMonad
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.WorkspaceHistory

-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook =
  -- fadeWindowsLogHook myFadeHook <+>
  workspaceHistoryHook

myFadeHook :: FadeHook
myFadeHook = composeAll [] --  [opaque, className =? "dmenu" --> transparency 0.5]
