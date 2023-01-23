module Core.MyLayoutHook where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W

-----------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

resizableTiled =
  mkToggle (single MIRROR) $
    mySpacing 10 10 $
      ResizableTall 1 (3 / 100) (1 / 2) []

threeColMid =
  mkToggle (single MIRROR) $
    mySpacing 10 10 $
      ResizableThreeColMid 1 (3 / 100) (1 / 2) []

grid = mySpacing 10 10 Grid

full = mySpacing 10 10 Full

-- Width of the window border in pixels.
--
myLayout = avoidStruts $ boringWindows lll -- \$ mouseResize $ windowArrange   lll
  where
    lll =
      resizableTiled
        ||| threeColMid
        ||| grid
        ||| full

data OnlyTiled = OnlyTiled
  deriving (Read, Show)

instance SetsAmbiguous OnlyTiled where
  hiddens _ _ _ mst wrs = filter (`elem` W.integrate' mst) $ map fst wrs
