module Utils where
import           XMonad
import           Data.Monoid
import qualified XMonad.StackSet as W
import           Data.IORef
import           Control.Monad
import           XMonad.Layout.Decoration
import           Data.Maybe
import           XMonad.Util.WindowProperties
import           XMonad.Hooks.EwmhDesktops
import           Data.List

myfullscreenEventHook :: Event -> X All
myfullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  managed <- isClient win
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> getProp32 wmstate win
  let isFull = fromIntegral fullsc `elem` wstate
      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      chWstate f = io
        $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)
  when (managed && typ == wmstate && fi fullsc `elem` dats)
    $ do
      when (action == add || (action == toggle && not isFull))
        $ do
          chWstate (fi fullsc:)
          windows $ W.float win $ W.RationalRect 0 0 1 1
      when (action == remove || (action == toggle && isFull))
        $ do
          chWstate $ delete (fi fullsc)
          windows $ W.sink win
  return $ All True
myfullscreenEventHook _ = return $ All True

toggleableFullscreen :: IORef Bool -> Event -> X All
toggleableFullscreen ref evt = io (readIORef ref)
  >>= \isOn -> if isOn
               then fullscreenEventHook evt
               else return (All True)
