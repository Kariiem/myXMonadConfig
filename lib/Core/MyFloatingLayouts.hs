{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Core.MyFloatingLayouts where

import XMonad
import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Util.Image
import XMonad.Layout.Renamed



floatLayout :: _ Window
floatLayout =
  renamed [Replace "Floating"] $
   fullDeco MyShrinker myFloatTheme $
    maximize $
      minimize $
        withBorder 3 $
          borderResize
            simplestFloat -- simpleFloat' shrinkText noDeco

myFloatTheme :: Theme
myFloatTheme =
  -- (Th.theme Th.adwaitaTheme)
  def
    { activeColor = "#5599cc",
      activeBorderColor = "#1f3999",
      activeTextColor = "white",
      inactiveColor = "#5599cc",
      inactiveBorderColor = "#198044",
      inactiveTextColor = "#73e6a3",
      fontName = "xft:hack",
      decoWidth = 200,
      decoHeight = 20,
      windowTitleIcons =
        [ (menuButton, CenterLeft 3),
          (closeButton, CenterRight 5),
          (maxiButton, CenterRight 20),
          (miniButton, CenterRight 35)
        ]
        -- windowTitleAddons =
        -- [ ("\xf85b", AlignLeft),
        -- ("\xf2d1", AlignRightOffset minimizeButtonOffset),
        -- ("\xf2d0", AlignRightOffset maximizeButtonOffset),
        -- ("\xf656", AlignRightOffset closeButtonOffset)
        -- ]
    }

fullDeco ::
  (Eq a, Shrinker s) =>
  s ->
  Theme ->
  l a ->
  ModifiedLayout (Decoration FullDeco s) l a
fullDeco s c = decoration s c $ FD True

newtype FullDeco a = FD Bool deriving (Show, Read)

instance Eq a => DecorationStyle FullDeco a where
  describeDeco _ = ""
  decorationCatchClicksHook _ = imageTitleBarButtonHandler
  decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()


data MyShrinker = MyShrinker

instance Show MyShrinker where show _ = ""
instance Read MyShrinker where readsPrec _ s = [(MyShrinker,s)]
instance Shrinker MyShrinker where 
    shrinkIt _ _ = [""]

buttonSize :: Int
buttonSize = 10

menuButtonOffset :: Int
menuButtonOffset = 4

minimizeButtonOffset :: Int
minimizeButtonOffset = 32

maximizeButtonOffset :: Int
maximizeButtonOffset = 18

closeButtonOffset :: Int
closeButtonOffset = 4

-- The images in a 0-1 scale to make
-- it easier to visualize

convertToBool' :: [Int] -> [Bool]
convertToBool' = map (== 1)

convertToBool :: [[Int]] -> [[Bool]]
convertToBool = map convertToBool'

menuButton' :: [[Int]]
menuButton' =
  [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
  ]

menuButton :: [[Bool]]
menuButton = convertToBool menuButton'

miniButton' :: [[Int]]
miniButton' =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

miniButton :: [[Bool]]
miniButton = convertToBool miniButton'

maxiButton' :: [[Int]]
maxiButton' =
  [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 0, 0, 0, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 0, 0, 0, 1, 1],
    [1, 1, 0, 0, 0, 0, 0, 0, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
  ]

maxiButton :: [[Bool]]
maxiButton = convertToBool maxiButton'

closeButton' :: [[Int]]
closeButton' = [[0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,1,1,0,0,0,0,1,1,0,0],
                [0,0,1,1,1,0,0,1,1,1,0,0],
                [0,0,0,1,1,1,1,1,1,0,0,0],
                [0,0,0,0,1,1,1,1,0,0,0,0],
                [0,0,0,0,1,1,1,1,0,0,0,0],
                [0,0,0,1,1,1,1,1,1,0,0,0],
                [0,0,1,1,1,0,0,1,1,1,0,0],
                [0,0,1,1,0,0,0,0,1,1,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0],
                [0,0,0,0,0,0,0,0,0,0,0,0]]
                

closeButton :: [[Bool]]
closeButton = map (map (==0)) closeButton'
