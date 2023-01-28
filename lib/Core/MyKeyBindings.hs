module Core.MyKeyBindings where

import Core.MyScratchpads
import Core.MyStatusBar
import Data.Map qualified as M
import System.Exit
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad

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
        ("M-<Tab>", addName "" $ windows W.focusDown),
        ("M-j", addName "" $ windows W.focusDown),
        ("M-k", addName "" $ windows W.focusUp),
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
        ("M-M1-l", addName "" $ sendMessage MirrorShrink),
        ("M-M1-h", addName "" $ sendMessage MirrorExpand),
        ("M-S-b", addName "" $ sendMessage ToggleStruts),
        ("M-s", addName "" $ toggleSpaces),
        ("M-b", addName "" $ sendMessage $ Toggle NOBORDERS),
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
        ("M-M1-<Left>", addName "" $ sendMessage (MoveLeft 10)),
        ("M-M1-<Right>", addName "" $ sendMessage (MoveRight 10)),
        ("M-M1-<Down>", addName "" $ sendMessage (MoveDown 10)),
        ("M-M1-<Up>", addName "" $ sendMessage (MoveUp 10)),
        ("M1-C-<Left>", addName "" $ sendMessage (IncreaseLeft 5)),
        ("M1-C-<Right>", addName "" $ sendMessage (IncreaseRight 5)),
        ("M1-C-<Down>", addName "" $ sendMessage (IncreaseDown 5)),
        ("M1-C-<Up>", addName "" $ sendMessage (IncreaseUp 5)),
        ("M1-S-<Left>", addName "" $ sendMessage (DecreaseLeft 5)),
        ("M1-S-<Right>", addName "" $ sendMessage (DecreaseRight 5)),
        ("M1-S-<Down>", addName "" $ sendMessage (DecreaseDown 5)),
        ("M1-S-<Up>", addName "" $ sendMessage (DecreaseUp 5))
      ]
        ++ [ ("M-" ++ m ++ show k, addName "" $ windows $ f i)
             | (i, k) <- zip (workspaces conf) [1 :: Int .. 9],
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
    [ ((modm, button1),                 \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster ),
      ((modm .|. controlMask, button1), \w -> focus w >> windows W.shiftMaster),
      ((modm .|. shiftMask, button1),   \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
      )
    ]

-- you may also bind events to the mouse scroll wheel (button4 and button5)
fileManager :: String
fileManager = "thunar"

xmonadRestartComm :: String
xmonadRestartComm = "killall xmobar; xmonad --recompile && xmonad --restart"

rofiCommand :: String
rofiCommand = "/home/karim/.config/rofi/launchers/misc/launcher.sh"

browser :: String
browser = "microsoft-edge-dev"

fileBrowser :: String
fileBrowser = "thunar"

pdfHistory :: String
pdfHistory = "~/script/pdf_history"

powerOpts :: String
powerOpts = "~/script/powerOptions"

doom_emacsclient :: String
doom_emacsclient = "~/script/run_emacs doom doom-emacs"

vanilla_emacsclient :: String
vanilla_emacsclient =  -- "emacsclient -c -a 'emacs'" -- 
    "~/script/run_emacs vanilla vanilla-emacs"

dmenu_run :: String
dmenu_run = "~/Suckless/bin/dmenu_run_history"

taskManager :: String
taskManager = "btm"

changeThemeScript = "$XDG_CONFIG_HOME/xmonad/scripts/select-theme"
