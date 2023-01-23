module Core.MyStartupHook where
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce
import Color.Theme 

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

