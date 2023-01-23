{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


----------------------------------------------------------------
----------------------------------------------------------------

import Data.List
import Data.Map qualified as M
import Data.Monoid
-----------------------------------------------------------------
-----------------------------------------------------------------

import FloatingLayouts qualified as FL
import SBar
import System.Exit
import Theme
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.MagicFocus
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-----------------------------------------------------------------
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

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf =
  (subtitle "Custom Keys" :) $
    mkNamedKeymap conf $
      [ ("M-S-<Return>", addName "" $ spawn $ terminal conf),
        ("M-x", addName "" $ spawn powerOpts),
        ("M-p", addName "" $ spawn dmenu_run),
        ("M-S-<Space>", addName "" $ setLayout $ layoutHook conf),
        ("M-n", addName "" $ refresh),
        ("M-C-a", addName "" $ windows copyToAll),
        ("M-S-a", addName "" $ killAllOtherCopies),
        ("M-<Tab>", addName "" $ focusDown),
        ("M-j", addName "" $ focusDown),
        ("M-k", addName "" $ focusUp),
        ("M-m", addName "" $ windows W.focusMaster),
        ("M-<Return>", addName "" $ windows W.swapMaster),
        ("M-S-j", addName "" $ windows W.swapDown),
        ("M-S-k", addName "" $ windows W.swapUp),
        ("M-S-c", addName "" $ kill1),
        ("M-d", addName "" $ spawn doom_emacsclient),
        ("M-v", addName "" $ spawn vanilla_emacsclient),
        ("M-o", addName "" $ spawn pdfHistory),
        ("M-<Space>", addName "" $ sendMessage NextLayout),
        ("M-r", addName "" $ sendMessage $ Toggle MIRROR),
        ("M-h", addName "" $ sendMessage Shrink),
        ("M-l", addName "" $ sendMessage Expand),
        ("M-S-l", addName "" $ sendMessage MirrorShrink),
        ("M-S-h", addName "" $ sendMessage MirrorExpand),
        ("M-b", addName "" $ sendMessage ToggleStruts),
        ("M-s", addName "" $ toggleSpaces),
        ("M-t", addName "" $ withFocused $ windows . W.sink),
        ("M-S-t", addName "" $ spawn changeThemeScript),
        ("M-,", addName "" $ sendMessage (IncMasterN 1)),
        ("M-.", addName "" $ sendMessage (IncMasterN (-1))),
        ("M-S-q", addName "" $ io exitSuccess),
        ("M-q", addName "" $ sbCleanupHook mySB >> spawn xmonadRestartComm),
        ("M-S-n", addName "" $ namedScratchpadAction scratchpads "notes"),
        ("M-<Right>", addName "" $ nextWS),
        ("M-<Left>", addName "" $ prevWS),
        ("M-S-<Right>", addName "" $ shiftToNext),
        ("M-S-<Left>", addName "" $ shiftToPrev),
        ("M-C-<Right>", addName "" $ shiftToNext >> nextWS),
        ("M-C-<Left>", addName "" $ shiftToPrev >> prevWS),
        ("M-S-/", addName "" $ withFocused (sendMessage . maximizeRestore)),
        ("M-i", addName "" $ withFocused minimizeWindow),
        ("M-S-i", addName "" $ withLastMinimized maximizeWindowAndFocus)
      ]
        ++ [ ("M-" ++ m ++ show k, addName "" $ windows $ f i)
             | (i, k) <- zip (workspaces conf) [1::Int .. 9],
               (f, m) <-
                 [ (W.greedyView, ""),
                   (W.shift, "S-"),
                   (copy, "C-")
                 ]
           ]

applications modm =
  [ ((modm, xK_d), spawn doom_emacsclient),
    ((modm, xK_f), spawn "alacritty -e nnn"),
    ((modm, xK_b), spawn browser),
    ((modm, xK_Alt_L), spawn fileBrowser)
  ]

spaceControl modm =
  [ ((modm, xK_KP_Add), incScreenWindowSpacing 5),
    ((modm, xK_KP_Subtract), decScreenWindowSpacing 5),
    ((modm, xK_KP_Insert), setScreenWindowSpacing 5)
  ]

toggleSpaces :: X ()
toggleSpaces = toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm .|. controlMask, button1), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm .|. shiftMask, button1),
        \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
      )
    ]

-- you may also bind events to the mouse scroll wheel (button4 and button5)
------------------------------------------------------------------------
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
myBorderWidth :: Dimension
myBorderWidth = 2

myLayout = avoidStruts $ boringWindows lll -- \$ mouseResize $ windowArrange   lll
  where
    lll =
      resizableTiled
        ||| threeColMid
        ||| grid
        ||| full
        ||| FL.floatLayout

data OnlyTiled = OnlyTiled
  deriving (Read, Show)

instance SetsAmbiguous OnlyTiled where
  hiddens _ _ _ mst wrs = filter (`elem` W.integrate' mst) $ map fst wrs

------------------------------------------------------------------------
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

------------------------------------------------------------------------
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
      -- , fadeWindowsEventHook
      swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True),
      minimizeEventHook,
      followOnlyIf (fmap not isLayoutFloat)
    ]

isLayoutFloat :: X Bool
isLayoutFloat = fmap (isSuffixOf "Floating") $ gets (description . W.layout . W.workspace . W.current . windowset)

--  <+> toggleableFullscreen fullScreenRef
------------------------------------------------------------------------
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

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "sxhkd"
  spawnOnOnce "sys-mon" "st -e btm"
  spawn "killall trayer"
  spawnOnce "nm-applet"
  spawnOnce "picom"

  spawn
    ( "sleep 2 && trayer --edge top --align right --distance 10 --distancefrom right --distance 5 --distancefrom top \
      \--widthtype request --padding 6 --SetDockType true --SetPartialStrut false --expand true --transparent true --alpha 0 \
      \--tint "
        ++ trayerColor
        ++ " --height 30"
    )

trayerColor = "0x" ++ tail (colorBack theme)

-- spawnOnOnce "2" browser
-- spawn "pcmanfm --desktop &"
------------------------------------------------------------------------
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask :: KeyMask
myModMask = mod4Mask

dwmBlue :: String
dwmBlue = "#2b4f98"

fileManager :: String
fileManager = "thunar"

xmonadRestartComm :: String
xmonadRestartComm = "killall xmobar; xmonad --recompile && xmonad --restart"

xmobarConfigPath :: String
xmobarConfigPath = "xmobar"

rofiCommand :: String
rofiCommand = "/home/karim/.config/rofi/launchers/misc/launcher.sh"

browser :: String
browser = "microsoft-edge-dev"

fileBrowser :: String
fileBrowser = "thunar"

myBin :: String
myBin = "~/bin/"

pdfHistory :: String
pdfHistory = "~/script/pdf_history"

powerOpts :: String
powerOpts = "~/script/powerOptions"

doom_emacsclient :: String
doom_emacsclient = "~/script/run_emacs doom-daemon default"

vanilla_emacsclient :: String
vanilla_emacsclient = "~/script/run_emacs vanilla-daemon vanilla"

dmenu_run :: String
dmenu_run = "~/Suckless/bin/dmenu_run_history"

dmenu_browse :: String
dmenu_browse = myBin ++ "dbrowse"

taskManager :: String
taskManager = "btm"

changeThemeScript = "$XDG_CONFIG_HOME/xmonad/scripts/select-theme"

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
-- Border colors for unfocused and focused windows, respectively.
--

myNormalBorderColor :: String
myNormalBorderColor = colorBlue theme

-- myNormalBorderColor = "#5599cc"

myFocusedBorderColor :: String
myFocusedBorderColor = colorOrange theme

-- myFocusedBorderColor = "#55ff99"

scratchpads =
  [ -- run htop in xterm, find it by title, use default floating window placement
    NS "notes" "st -e nvim" (title =? "notes") defaultFloating,
    NS
      "stardict"
      "stardict"
      (className =? "Stardict")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]
