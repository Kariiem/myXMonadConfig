module Core.MyMangeHook where

import Core.MyScratchpads
import Data.Map qualified as M
import Data.Monoid
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ manageSpawn,
      insertPosition Below Newer,
      placeHook $ withGaps (16, 16, 16, 16) (smart (0.5, 0.5)), -- simpleSmart -- (smart (0.5,0.5))
      namedScratchpadManageHook scratchpads,
      className =? "jetbrains-idea-ce" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "download" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "Xmessage" --> doFloat
    ]

-- , liftX current_is_floating --> hasBorder True -- Borders around floating windows
--  , className =? "VScodium" --> doFloat
-- , className =? "Gimp" --> doFloat
-- , resource =? "desktop_window" --> doIgnore
-- , resource =? "kdesktop" --> doIgnore

current_is_floating :: X Bool
current_is_floating = do
  wins <- gets windowset
  return $
    case W.peek wins of
      Just w -> M.member w (W.floating wins)
      Nothing -> False
