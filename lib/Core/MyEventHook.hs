module Core.MyEventHook where

import Data.Monoid
import XMonad
import XMonad.Hooks.WindowSwallowing
import XMonad.Util.Hacks qualified as Hacks

-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook =
  composeAll
    [ Hacks.windowedFullscreenFixEventHook,
      swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)
    ]


