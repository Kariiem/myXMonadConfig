{-# LANGUAGE ImportQualifiedPost, TypeSynonymInstances, MultiParamTypeClasses #-}

  -- Builtin
import Data.Map qualified as M
import Data.Monoid
import System.Exit
import Text.Printf
import System.IO
import Control.Arrow (first, (&&&))
import Data.Ord

  -- Basic
import XMonad
import XMonad.Prelude
import XMonad.StackSet qualified as W

  -- Layouts
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows qualified as BW
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.Simplest

  -- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.MouseResize
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize

  -- Hooks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

  -- Utils
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedWindows

--
myTerminal :: String
myTerminal = "st" --"alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask :: KeyMask
myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor = "#5599cc"

myFocusedBorderColor :: String
myFocusedBorderColor = "#55ff99"

myBorderWidth :: Dimension
myBorderWidth = 2

defaultGapSize :: Integer
defaultGapSize = 10

myWorkspaces :: [String]
myWorkspaces = map show [1..9] -- ["gsoc", "nix", "ghc", "dev", "www", "xmonad", "sys"]

myLogHook :: X ()
myLogHook =
  workspaceHistoryHook

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ manageSpawn
    , insertPosition Below Newer
    ]

resizableTiled = renamed [Replace "tall"]
               $ ResizableTall 1 (3 / 100) (1 / 2) []

threeColMid = renamed [Replace "threeColMid"]
            $ ResizableThreeColMid 1 (3 / 100) (1 / 2) []

threeCol = renamed [Replace "threeCol"]
         $ ResizableThreeCol 1 (3 / 100) (1 / 2) []

tabLayout = renamed [Replace "tabs"]
          $ tabbed shrinkText tabLayoutTheme

full = renamed [Replace "monocle"] Full

myLayout = avoidStruts
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT)
         . mkToggle (single MIRROR )
         . renamed [KeepWordsRight 1]
         . minimize
         . BW.boringWindows
         $ lll -- . avoidStruts lll
  where
    lll =
            resizableTiled
        ||| threeCol
        ||| threeColMid
        ||| tabLayout
        ||| full

tabLayoutTheme :: Theme
tabLayoutTheme = def { activeColor = "#4455ee"
                     , inactiveColor = "#555555"
                     , activeTextColor = "#aceffe"
                     , inactiveTextColor = "#122134"
                     , fontName = "xft:Ubuntu:bold"
                     , inactiveBorderWidth = 0
                     , activeBorderWidth = 0
                     , urgentBorderWidth = 0
                     , decoHeight = 30
                     }


myKeys conf =
               [ ("M-q"          , spawn "xmonad --restart")
               , ("M-r"          , spawn "xmonad --recompile && dunstify -t 300 'XMonad recompiled successfully'")
               , ("M-S-c"        , kill1)
               , ("M-S-q"        , io exitSuccess)
               , ("M-S-r"        , refresh)
               , ("M-S-b"        , sendMessage ToggleStruts)
               , ("M-<Space>"    , spawn "~/.xmonad/scripts/run-recent -l 10" )
               , ("M-<Return>"   , spawn (terminal conf))
               , ("M-f f"        , toggleFull)
               , ("M-x"          , spawn "~/.xmonad/scripts/poweropts")
               , ("M-e"          , spawn "emacs")
               , ("M-S-<Tab>"    , setLayout $ layoutHook conf)
               , ("M-<Tab>"      , sendMessage NextLayout)
               , ("M-S-m"        , sendMessage $ Toggle MIRROR)
               , ("M-t s"        , toggleSpaces)
               , ("M-t b"        , sendMessage $ Toggle NOBORDERS)
               , ("M-t S-b"      , (broadcastMessage $ Toggle NOBORDERS) >> refresh)
               , ("M-s"          , withFocused $ windows . W.sink)
               , ("M-,"          , sendMessage (IncMasterN 1))
               , ("M-."          , sendMessage (IncMasterN (-1)))
               , ("M-k"          , BW.focusDown)
               , ("M-j"          , BW.focusUp)
               , ("M-S-<Return>" , windows W.swapMaster)
               , ("M-S-k"        , BW.swapDown)
               , ("M-S-j"        , BW.swapUp)
               , ("M-h"          , sendMessage Shrink)
               , ("M-l"          , sendMessage Expand)
               , ("M-S-l"        , sendMessage MirrorShrink)
               , ("M-S-h"        , sendMessage MirrorExpand)
               , ("M-S-<Right>"  , shiftToNext)
               , ("M-S-<Left>"   , shiftToPrev)
               , ("M-C-<Right>"  , shiftToNext >> nextWS)
               , ("M-C-<Left>"   , shiftToPrev >> prevWS)
               , ("M-a"         , prevWS)
               , ("M-d"         , nextWS)
               , ("M-<Left>"     , prevWS)
               , ("M-<Right>"    , nextWS)
               ]
               ++
               [ ("M-"   ++ show i, windows . W.greedyView $ ws !! (i-1)) | i <- [1..length myWorkspaces]]
               ++
               [ ("M-S-" ++ show i, windows . W.shift $ ws !! (i-1))      | i <- [1..length myWorkspaces]]
               ++
               [ ("M-C-" ++ show i, windows . copy $ ws !! (i-1))         | i <- [1..length myWorkspaces]]
               ++
               [ ("<XF86AudioMute>"       , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
               , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
               , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
               ]
     where
          ws = workspaces conf

toggleSpaces :: X ()
toggleSpaces = toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1)                 , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster )
    , ((modm .|. controlMask, button1) , \w -> focus w >> windows W.shiftMaster)
    , ((modm .|. shiftMask, button1)   , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

toggleFull = sendMessage (Toggle FULL) >> sendMessage ToggleStruts

main :: IO ()
main = xmonad
     . docks
     . ewmh
     . ewmhFullscreen
     $ myXConfig

myXConfig = def
  { terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses = myClickJustFocuses
  , borderWidth = myBorderWidth
  , modMask = myModMask
  , workspaces =  myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys = \conf -> mkKeymap conf $ myKeys conf
  , mouseBindings = myMouseBindings
  , layoutHook = myLayout
  , manageHook = myManageHook
  , handleEventHook = swallowEventHook (className =? "st-256color") (return True) -- <||> className =? "Alacritty") (return True)
  , logHook = workspaceHistoryHook
  , startupHook = do
      spawnOnce trayer
      spawnOnce "polybar"
      spawnOnOnce "9" "st -e btm"
      setWMName "LG3D"
  }

trayer = "trayer --edge top \
         \--width 10 \
         \--distancefrom top \
         \--padding 6 \
         \--SetDockType true \
         \--SetPartialStrut false \
         \--expand true \
         \--transparent true \
         \--tint '#181818' \
         \--alpha 0"
