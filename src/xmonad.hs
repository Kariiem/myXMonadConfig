{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

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
import XMonad.Util.NamedScratchpad
import XMonad.Util.Hacks qualified as Hacks
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedWindows
import XMonad.Util.Dmenu

  -- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

  -- MyLib
import Color.Theme

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

myNormalBorderColor :: String
myNormalBorderColor = "#5599cc"

myFocusedBorderColor :: String
myFocusedBorderColor = "#55ff99"

myBorderWidth :: Dimension
myBorderWidth = 2

defaultGapSize :: Integer
defaultGapSize = 10


-- each string names in the ScriptSection list is name of the script to be called for action
scripts = M.fromList [ ("dmenu","$XDG_CONFIG_HOME/scripts/dmenu/")
                     ,  ("yad", "$XDG_CONFIG_HOME/scripts/yad/")
                     ,  ("misc", "$XDG_CONFIG_HOME/scripts/misc/")
                     ]

scriptPath section name = (scripts M.! section) ++ name

volumeControls = M.fromList [ ("inc", "pactl set-sink-volume @DEFAULT_SINK@ +1000")
                            , ("dec", "pactl set-sink-volume @DEFAULT_SINK@ -1000")
                            , ("tog", "pactl set-sink-mute @DEFAULT_SINK@ toggle")
                            ]

sysMonitor :: String
sysMonitor = "btm"

myWorkspaces :: [String]
myWorkspaces = ["fecu1","fecu2","fecu3","docs","www","dev","freebsd","sys-mon"] --map show [1..9::Int]

scratchpads =
  [ -- run htop in xterm, find it by title, use default floating window placement
    NS "notes" "st -e nvim" (title =? "notes") defaultFloating,
    NS
      "stardict"
      "stardict"
      (className =? "Stardict")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

myLogHook :: X ()
myLogHook =
  workspaceHistoryHook

myFadeHook :: FadeHook
myFadeHook = composeAll []

myPP :: PP
myPP =
  def
    { ppCurrent = xmobarColor (colorRed theme) "" . xmobarBorder "Bottom" (colorRed theme) 0  ,
      ppUrgent = xmobarColor (colorGreen theme) (colorBPurple theme),
      ppLayout = xmobarColor (colorBPurple theme) "" ,
      ppSep = " ",
      ppWsSep = " ",
      ppExtras = [activeWindowCount, hiddenWindowCount],
      ppTitle = xmobarColor (colorFore theme) "" . shorten 40,
      ppHidden = xmobarColor (colorCyan theme) "",
      ppHiddenNoWindows = xmobarColor (colorGrey theme) "",
      ppOrder = \(ws : l : t : ex) -> ws : ex ++ l : [t]
    }

windowCount :: X (Maybe String)
windowCount =
  Just
    . xmobarColor (colorGreen theme) ""
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset
    <$> get

activeWindowCount =  do
  w <- length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get
  h <- length <$> minimizedWindows
  return . Just . xmobarColor (colorGreen theme) "" . show $ w - h

hiddenWindowCount =   Just
    . ("/ "++)
    . xmobarColor (colorYellow theme) ""
    . show
    . length
    <$> minimizedWindows

mySB :: StatusBarConfig
mySB =
  statusBarProp "xmobar" $
    copiesPP (xmobarFont 4 . xmobarColor (colorBlack theme) "") myPP >>= clickablePP

myStartupHook :: X ()
myStartupHook = do
  -- return () >> yadCheckKeymap customXConfig
  --            $ concatMap (\(KeySection _ keys) -> keys)
  --            $ myKeysSections customXConfig -- customXConfig = myXConfig {layoutHook = Layout $ layoutHook myXConfig}
  -- this is equivalent to the above, using the <*> operator over functions, which acts as the `S` combinator.
  return () >> yadCheckKeymap
               <*> (concatMap (\(KeySection _ keys) ->keys) . myKeysSections)
               $ myXConfig {layoutHook = Layout $ layoutHook myXConfig}
  setWMName "LG3D"
  -- spawnOnce "sxhkd"
  -- spawnOnce "emacs --with-profile doom-emacs --daemon &"
  -- spawnOnce "emacs --with-profile vanilla-emacs --daemon &"
  spawnOnce "lxsession -e XMonad -a -n"
  spawnOnce "nm-applet"
  spawnOnce "blueman-applet"
  spawnOnce "picom"
  -- spawnOnce "alttab -w 1 -d 1"
  spawnOnOnce "sys-mon" ("st -e " ++ sysMonitor)
  spawn trayer2


trayer1 = "killall trayer ; sleep 2 && trayer --edge top \
          \--align right \
          \--distance 10 \
          \--distancefrom right \
          \--distance 5 \
          \--distancefrom top \
          \--widthtype request \
          \--padding 6 \
          \--SetDockType true \
          \--SetPartialStrut false \
          \--expand true \
          \--transparent true \
          \--alpha 0 \
          \--tint "
        ++ trayer1Color
        ++ " --height 25"
trayer2 = "killall stalonetray ; sleep 2 && stalonetray \
          \--sticky true \
          \--dockapp-mode none \
          \--icon-size 24 \
          \--grow-gravity E \
          \--icon-gravity SE \
          \--kludges force_icons_size \
          \--window-type dock \
          \--geometry 1x1-15+5 \
          \--background " ++ trayer2Color
trayer1Color = "0x" ++ tail (colorBack theme)
trayer2Color = show $ colorBack theme

myEventHook :: Event -> X All
myEventHook =
  composeAll
    [ Hacks.windowedFullscreenFixEventHook
    , swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)
    , stalonetrayAboveXmobarEventHook
    , stalonetrayPaddingXmobarEventHook
    ]
stalonetrayAboveXmobarEventHook = Hacks.trayAbovePanelEventHook (className =? "stalonetray") (appName =? "xmobar")
stalonetrayPaddingXmobarEventHook = Hacks.trayPaddingXmobarEventHook (className =? "stalonetray") "_XMONAD_STRAYPAD"

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ manageSpawn
    , insertPosition Below Newer
    , namedScratchpadManageHook scratchpads
--    , className =? "jetbrains-idea-ce"  --> doFloat
    , className =? "dialog"             --> doFloat
    , className =? "download"           --> doFloat
    , className =? "notification"       --> doFloat
    , className =? "Xmessage"           --> doFloat
    , className =? "Yad"                --> doCenterFloat
    , className =? "Qalculate-gtk"      --> doCenterFloat
    -- The following line causes the trayer (stalonetray) to hide on <toggleStruts>
    -- and on full screen events
    , className =? "stalonetray"
      <||> className =? "trayer"
      <||> className =? "panel"         --> doLower
    , className =? "Screenkey"          --> doFloat
    , placeHook $ withGaps (16, 16, 16, 16) (smart (0.5, 0.5))
    ]

mySpacing :: Integer -> Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i j = spacingRaw False (Border i i i i) True (Border j j j j) True

resizableTiled = renamed [Replace "tall"]
               $ mySpacing defaultGapSize defaultGapSize
               $ ResizableTall 1 (3 / 100) (1 / 2) []

threeColMid = renamed [Replace "threeColMid"]
            $ mySpacing defaultGapSize defaultGapSize
            $ ResizableThreeColMid 1 (3 / 100) (1 / 2) []

threeCol = renamed [Replace "threeCol"]
         $ mySpacing defaultGapSize defaultGapSize
         $ ResizableThreeCol 1 (3 / 100) (1 / 2) []

tabLayout = renamed [Replace "tabs"]
          $ tabbed shrinkText tabLayoutTheme

grid = renamed [Replace "grid"]
     $ mySpacing defaultGapSize defaultGapSize Grid

full = renamed [Replace "monocle"]
     $ mySpacing defaultGapSize defaultGapSize Full

myFloat = renamed [Replace "float"]
     --   $ mouseResize
     --   $ borderResize
     --   $ windowArrangeAll
        $ simpleFloat' shrinkText floatLayoutTheme

myLayout = avoidStruts
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT )
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
        ||| grid
        ||| full
        ||| myFloat

tabLayoutTheme :: Theme
tabLayoutTheme = def { activeColor = colorBlue theme
                     , inactiveColor = colorGrey theme
                     , activeTextColor = colorFore theme
                     , inactiveTextColor = colorFore theme
                     , fontName = "xft:Ubuntu:bold"
                     , inactiveBorderWidth = 0
                     , activeBorderWidth = 0
                     , urgentBorderWidth = 0
                     , decoHeight = 30
                     }

floatLayoutTheme = def { activeColor = colorBlue theme
                     , inactiveColor = colorGrey theme
                     , activeTextColor = colorFore theme
                     , inactiveTextColor = colorFore theme
                     , fontName = "xft:Ubuntu:bold"
                     , inactiveBorderWidth = 0
                     , activeBorderWidth = 0
                     , urgentBorderWidth = 0
                     , decoHeight = 30
                     }

myXPConfig = def { bgColor = "#043850"
                 , fgColor = "#ea96fa"
                 , font = "xft:Hack:size=12"
                 , position = CenteredAt 0.3 (fromRational 2/3)
                 , promptBorderWidth = 0
                 , height = 30
                 , maxComplRows = Just 10
                 }
data KeySection = KeySection String {-Title-} [(String,NamedAction)] {- keys -}

myKeysSections :: XConfig Layout -> [KeySection]
myKeysSections conf =
  [ KeySection "XMonad Controls"
               [ ("M-q"          , addName "\tRestart XMonad"                $ sbCleanupHook mySB >> spawn "xmonad --restart")
               , ("M-r"          , addName "\tRecompile XMonad"              $ spawn "xmonad --recompile && dunstify -t 300 'XMonad recompiled successfully'")
               , ("M-S-c"        , addName "\tKill the focused application"  $ kill1)
               , ("M-S-q"        , addName "\tExit XMonad"                   $ io exitSuccess)
               , ("M-S-r"        , addName "\tRefresh XMonad"                $ refresh)
               , ("M-S-b"        , addName "\tShow/Hide status bar"          $ sendMessage ToggleStruts)
               ]
  , KeySection "Dmenu & YAD Scripts"
               [ ("M-<Space>"    , addName "\tDmenu app launcher"            $ spawn $ scriptPath "dmenu" "run-recent" )
              -- ("M-<Space>"    , addName "\tXMonad Shell Prompt"           $ shellPrompt myXPConfig)
               , ("M-d c"        , addName "\tChange color theme"            $ spawn $ scriptPath "dmenu" "theme" )
               , ("M-d x"        , addName "\tExit prompt "                  $ spawn $ scriptPath "dmenu" "poweropts")
               , ("M-d p"        , addName "\tPdf history"                   $ spawn $ scriptPath "dmenu" "pdfhist")
               , ("M-d d"        , addName "\tDictionary:word meaning"       $ spawn $ scriptPath "yad"   "dictionary")
               , ("M-d u"        , addName "\tSystem Update"                 $ spawn $ scriptPath "yad"   "update")
               , ("M-d s"        , addName "\tScreenshot"                    $ spawn $ scriptPath "dmenu" "screenshot")
               , ("M-p"          , addName "\tPassmenu"                      $ spawn $ scriptPath "dmenu" "pass")
               ]
  , KeySection "Applications"
               [ ("M-<Return>"   , addName ("\tOpen a new terminal ("++myTerminal++")") $ spawn (terminal conf))
               , ("M-e d"        , addName "\tLaunch Doom Emacs"                        $ spawn $ scriptPath "misc" "doom")
               , ("M-e v"        , addName "\tLaunch vanilla Emacs"                     $ spawn $ scriptPath "misc" "vanilla")
               , ("M-f m"        , addName "\tLaunch fff file manager"                  $ spawn "st -e fff")
            -- this is here due to a limitation of the key section scheme
               , ("M-f f"        , addName "\tMake the focused window Fullscreen"       $ toggleFull)
               ]
  , KeySection "Layout Controls"
               [ ("M-S-<Tab>"    , addName "\tReset the window layout"             $ setLayout $ layoutHook conf)
               , ("M-<Tab>"      , addName "\tNext layout"                         $ sendMessage NextLayout)
               , ("M-S-m"        , addName "\tRotate layout by 90 degrees"         $ sendMessage $ Toggle MIRROR)
               , ("M-t s"        , addName "\tToggle gaps"                         $ toggleSpaces)
               , ("M-t b"        , addName "\tToggle borders"                      $ sendMessage $ Toggle NOBORDERS)
               , ("M-s"          , addName "\tSink a floating window"              $ withFocused $ windows . W.sink)
               , ("M-,"          , addName "\tIncrease windows in the master pane" $ sendMessage (IncMasterN 1))
               , ("M-."          , addName "\tDecrease windows in the master pane" $ sendMessage (IncMasterN (-1)))
               , ("M-S-n"        , addName "\tOpen a scratchpad"                   $ namedScratchpadAction scratchpads "notes")
               ]
  , KeySection "Window Controls"
               [ ("M-C-a"        , addName "\tCopy the focused window to all workspaces" $ windows copyToAll)
               , ("M-S-a"        , addName "\tKill all copies of the focused window"     $ killAllOtherCopies)
               , ("M-k"          , addName "\tFocus the next window"                     $ BW.focusDown)
               , ("M-j"          , addName "\tFocus the previous window"                 $ BW.focusUp)
               , ("M-S-<Return>" , addName "\tSwap the focused window with the master window"   $ windows W.swapMaster)
               , ("M-S-k"        , addName "\tSwap the focused window with the next window"     $ BW.swapDown)
               , ("M-S-j"        , addName "\tSwap the focused window with the previous window" $ BW.swapUp)
               , ("M-h"          , addName "\tShrink window"       $ sendMessage Shrink)
               , ("M-l"          , addName "\tExpand window"       $ sendMessage Expand)
               , ("M-S-l"        , addName "\tMirrorShrink window" $ sendMessage MirrorShrink)
               , ("M-S-h"        , addName "\tMirrorExpand window" $ sendMessage MirrorExpand)
               , ("M-<Backspace>", addName "\tHide the current window" $  withFocused minimizeWindow)
               , ("M-S-<Backspace>" , addName "\tRestore the oldest hidden window" $ withLastMinimized maximizeWindow)
               , ("M-C-<Backspace>" , addName "\tShow all the hidden windows"      $ listAllMinimized)
               , ("M-S-<Right>"  , addName "\tShift window to next workspace"             $ shiftToNext)
               , ("M-S-<Left>"   , addName "\tShift window to prev workspace"             $ shiftToPrev)
               , ("M-C-<Right>"  , addName "\tShift window and focus to next workspace"   $ shiftToNext >> nextWS)
               , ("M-C-<Left>"   , addName "\tShift window and focus to prev workspace"   $ shiftToPrev >> prevWS)
               ]
  , KeySection "Floating Layouts Controls"
               [ ("M-M1-<Left>"  , addName "\tMove window left by 10 pixels"      $ sendMessage (MoveLeft 10))
               , ("M-M1-<Right>" , addName "\tMove window right by 10 pixels"     $ sendMessage (MoveRight 10))
               , ("M-M1-<Down>"  , addName "\tMove window down by 10 pixels"      $ sendMessage (MoveDown 10))
               , ("M-M1-<Up>"    , addName "\tMove window up by 10 pixels"        $ sendMessage (MoveUp 10))
               , ("M1-C-<Left>"  , addName "\tExpand the left edge by 5 pixels"   $ sendMessage (IncreaseLeft 5))
               , ("M1-C-<Right>" , addName "\tExpand the right edge by 5 pixels"  $ sendMessage (IncreaseRight 5))
               , ("M1-C-<Down>"  , addName "\tExpand the bottom edge by 5 pixels" $ sendMessage (IncreaseDown 5))
               , ("M1-C-<Up>"    , addName "\tExpand the top edge by 5 pixels"    $ sendMessage (IncreaseUp 5))
               , ("M1-S-<Left>"  , addName "\tShrink the left edge by 5 pixels"   $ sendMessage (DecreaseLeft 5))
               , ("M1-S-<Right>" , addName "\tShrink the right edge by 5 pixels"  $ sendMessage (DecreaseRight 5))
               , ("M1-S-<Down>"  , addName "\tShrink the bottom edge by 5 pixels" $ sendMessage (DecreaseDown 5))
               , ("M1-S-<Up>"    , addName "\tShrink the top edge by 5 pixels"    $ sendMessage (DecreaseUp 5))
               ]
  , KeySection "Workspace Controls"
               [ ("M-<Right>"    , addName "\tGoto next workspace"                $ nextWS)
               , ("M-<Left>"     , addName "\tGoto previous workspace"            $ prevWS)
               , ("M-1"          , addName ("\tGoto workspace 1")                 $ windows $ W.greedyView $ ws !! 0)
               , ("M-2"          , addName ("\tGoto workspace 2")                 $ windows $ W.greedyView $ ws !! 1)
               , ("M-3"          , addName ("\tGoto workspace 3")                 $ windows $ W.greedyView $ ws !! 2)
               , ("M-4"          , addName ("\tGoto workspace 4")                 $ windows $ W.greedyView $ ws !! 3)
               , ("M-5"          , addName ("\tGoto workspace 5")                 $ windows $ W.greedyView $ ws !! 4)
               , ("M-6"          , addName ("\tGoto workspace 6")                 $ windows $ W.greedyView $ ws !! 5)
               , ("M-7"          , addName ("\tGoto workspace 7")                 $ windows $ W.greedyView $ ws !! 6)
               , ("M-8"          , addName ("\tGoto workspace 8")                 $ windows $ W.greedyView $ ws !! 7)
               , ("M-9"          , addName ("\tGoto workspace 9")                 $ windows $ W.greedyView $ ws !! 8)
               , ("M-S-1"        , addName ("\tShift window to workspace 1")      $ windows $ W.shift $ ws !! 0)
               , ("M-S-2"        , addName ("\tShift window to workspace 2")      $ windows $ W.shift $ ws !! 1)
               , ("M-S-3"        , addName ("\tShift window to workspace 3")      $ windows $ W.shift $ ws !! 2)
               , ("M-S-4"        , addName ("\tShift window to workspace 4")      $ windows $ W.shift $ ws !! 3)
               , ("M-S-5"        , addName ("\tShift window to workspace 5")      $ windows $ W.shift $ ws !! 4)
               , ("M-S-6"        , addName ("\tShift window to workspace 6")      $ windows $ W.shift $ ws !! 5)
               , ("M-S-7"        , addName ("\tShift window to workspace 7")      $ windows $ W.shift $ ws !! 6)
               , ("M-S-8"        , addName ("\tShift window to workspace 8")      $ windows $ W.shift $ ws !! 7)
               , ("M-S-9"        , addName ("\tShift window to workspace 9")      $ windows $ W.shift $ ws !! 8)
               , ("M-C-1"        , addName ("\tCopy window to workspace 1")       $ windows $ copy $ ws !! 0)
               , ("M-C-2"        , addName ("\tCopy window to workspace 2")       $ windows $ copy $ ws !! 1)
               , ("M-C-3"        , addName ("\tCopy window to workspace 3")       $ windows $ copy $ ws !! 2)
               , ("M-C-4"        , addName ("\tCopy window to workspace 4")       $ windows $ copy $ ws !! 3)
               , ("M-C-5"        , addName ("\tCopy window to workspace 5")       $ windows $ copy $ ws !! 4)
               , ("M-C-6"        , addName ("\tCopy window to workspace 6")       $ windows $ copy $ ws !! 5)
               , ("M-C-7"        , addName ("\tCopy window to workspace 7")       $ windows $ copy $ ws !! 6)
               , ("M-C-8"        , addName ("\tCopy window to workspace 8")       $ windows $ copy $ ws !! 7)
               , ("M-C-9"        , addName ("\tCopy window to workspace 9")       $ windows $ copy $ ws !! 8)
               ]
  , KeySection "Gap Controls"
               [ ("M-g i"        , addName "\tIncrease gap size by 5 pixels"      $ incScreenWindowSpacing 5)
               , ("M-g d"        , addName "\tdecrease gap size by 5 pixels"      $ decScreenWindowSpacing 5)
               , ("M-g r"        , addName "\tReset gap size to `defaultGapSize`" $ setScreenWindowSpacing 10)
               ]
  , KeySection "Fn Keys and Others"
               [ ("<XF86AudioRaiseVolume>"     , addName "\tInc Volume"          $ spawn $ volumeControls M.! "inc")
               , ("<XF86AudioLowerVolume>"     , addName "\tDec Volume"          $ spawn $ volumeControls M.! "dec")
               , ("<XF86AudioMute>"            , addName "\tToggle Volume"       $ spawn $ volumeControls M.! "tog")
               , ("<XF86MonBrightnessUp>"      , addName "\tInc Brightness"      $ spawn $ scriptPath "misc" "bright inc")
               , ("<XF86MonBrightnessDown>"    , addName "\tDec Brightness"      $ spawn $ scriptPath "misc" "bright dec")
               , ("M-<Print>"                  , addName "\tTake a Screnshot"    $ spawn "maim -u ~/Pictures/Screenshots/\"$(date)\".png")
               , ("<XF86AudioPlay>"            , addName "\tResume/Pause"        $ spawn "mocp --toggle-pause"    )
               ]
  ]
     where
          ws = workspaces conf

myKeys conf = concatMap (\(KeySection title keys) -> subTitle title keys) (myKeysSections conf)
      where
        subTitle str keys = (subtitle str) : mkNamedKeymap conf keys

toggleSpaces :: X ()
toggleSpaces = toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1)                 , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster )
    , ((modm .|. controlMask, button1) , \w -> focus w >> windows W.shiftMaster)
    , ((modm .|. shiftMask, button1)   , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

minimizedWindows = withMinimized return

listAllMinimized = minimizedWindows >>= mapM winInfo >>= action
   where
     winInfo w = do
                    name <- getNameWMClass w
                    title <- getName w
                    let str = printf "%-60s %-60s %s" ("["++ show w ++"]") (show name) (show title)
                    return str -- (w, title, name)

     action ws = if null ws
                 then io . spawn $ "notify-send -t 400 \"There are no hidden windows\""
                 else do
                        selString  <- io . myDmenu . sort $ ws
                        let winId:_ = read (takeWhile (not.isSpace) selString):: [Window]
                        maximizeWindow winId

     myDmenu = menuArgs "dmenu" ["-z","1910","-l","10","-p","select window: "]




data MyTransformers = MFULL deriving (Read,Show,Eq)

instance Transformer MyTransformers Window where
  transform MFULL x k = k full (const x)

toggleFull = sendMessage (Toggle FULL) >> sendMessage ToggleStruts

yad = "yad --undecorated --no-buttons --text-info --text-align=left --fontname=\"Hack 12\" --fore="
    ++ colorBBlue theme ++ " --back=" ++ colorBlack theme ++ " --geometry=1400x800"

pipeToYad str = do
            yadPipe <- spawnPipe yad
            hPutStrLn yadPipe str
            hClose yadPipe
            return ()


yadShowKeymaps x = addName "Show Keybindings" . io . pipeToYad . unlines . showKm $ x

yadCheckKeymap conf km = warn (doKeymapCheck conf km)
  where warn ([],[])   = return ()
        warn (bad,dup) = io . pipeToYad $ "Warning:\n"
                            ++ msg "bad" bad ++ "\n"
                            ++ msg "duplicate" dup
        msg _ [] = ""
        msg m xs = m ++ " keybindings detected: " ++ showBindings xs
        showBindings = unwords . map (("\""++) . (++"\""))

doKeymapCheck conf km = (bad,dups)
  where ks = map ((readKeySequence conf &&& id) . fst) km
        bad = nub . map snd . filter (isNothing . fst) $ ks
        dups = map (snd . head)
             . filter ((>1) . length)
             . groupBy ((==) `on` fst)
             . sortBy (comparing fst)
             . map (first fromJust)
             . filter (isJust . fst)
             $ ks

main :: IO ()
main = do
    xmonad
      . withSB mySB
      . docks
      . ewmh
      . ewmhFullscreen
      $ addDescrKeys' ((mod4Mask, xK_F1), yadShowKeymaps) myKeys myXConfig

myXConfig = def
  { terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses = myClickJustFocuses
  , borderWidth = myBorderWidth
  , modMask = myModMask
  , workspaces =  myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  -- , keys = myKeys
  , mouseBindings = myMouseBindings
  , layoutHook = myLayout
  , manageHook = myManageHook
  , handleEventHook = myEventHook
  , logHook = myLogHook
  , startupHook = myStartupHook
  }
