module Core.MyLayoutHook where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.BorderResize
import XMonad.Layout.PositionStoreFloat
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

resizableTiled = renamed [Replace "tall"] 
               $ mySpacing 10 10 
               $ ResizableTall 1 (3 / 100) (1 / 2) []

threeColMid = renamed [Replace "three col mid"]
            $ mySpacing 10 10 
            $ ResizableThreeColMid 1 (3 / 100) (1 / 2) []

threeCol = renamed [Replace "three col"]
            $ mySpacing 10 10 
            $ ResizableThreeCol 1 (3 / 100) (1 / 2) []


grid = renamed [Replace "grid"] $ mySpacing 10 10 Grid

full = renamed [Replace "monocle"] $ mySpacing 10 10 Full

myFloat = renamed [Replace "float"] 
        . mouseResize
        . borderResize 
        . windowArrangeAll 
        $  simplestFloat
-- Width of the window border in pixels.
--
myLayout = avoidStruts
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT)
         . mkToggle (single MIRROR) $ lll -- . avoidStruts lll
  where
    lll =
            resizableTiled
        ||| threeCol
        ||| threeColMid
        ||| grid
        ||| full
        ||| myFloat

data OnlyTiled = OnlyTiled
  deriving (Read, Show)

instance SetsAmbiguous OnlyTiled where
  hiddens _ _ _ mst wrs = filter (`elem` W.integrate' mst) $ map fst wrs
