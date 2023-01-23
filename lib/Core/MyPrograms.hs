module Core.MyPrograms where

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
