{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Data.List
import Data.Maybe
import Data.Unique
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.Trans.Class

import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph
import Graphics.UI.GIGtkStrut
import System.Taffybar.Information.X11DesktopInfo
import qualified System.Taffybar.Context as BC
import System.Taffybar.Context hiding (TaffybarConfig(..), BarConfig(..))
import System.Taffybar.Util
import GI.Gdk

newtype SimpleBarConfigs = SimpleBarConfigs (MV.MVar [(Int, BC.BarConfig)])

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

barH = 25
ypad = 5
xpad = 10
widspace = 10
myStrutConfig mon =
  defaultStrutConfig { strutHeight = ExactSize barH
                     , strutYPadding = ypad
                     , strutXPadding = xpad
                     , strutAlignment = Center
                     , strutPosition = TopPos
                     , strutMonitor = Just $ fromIntegral mon
                     }
myBarConfig mon = do
  barId <- newUnique
  return
   BC.BarConfig { BC.strutConfig = myStrutConfig mon
                , BC.widgetSpacing = widspace
                , BC.startWidgets = [workspaces]
                , BC.centerWidgets = []
                , BC.endWidgets = [clock,cpu]
                , BC.barId = barId
                }
myTaffyConfig =
    def
    { BC.getBarConfigsParam = configGetter
    }
  where
    useAllMonitors :: TaffyIO [Int]
    useAllMonitors = lift $ do
        count <- getMonitorCount
        return [0..count-1]
    getMonitorCount :: IO Int
    getMonitorCount =
      fromIntegral <$> (screenGetDefault >>= maybe (return 0)
                         (screenGetDisplay >=> displayGetNMonitors))
    configGetter = do
      SimpleBarConfigs configsVar <- getStateDefault $ lift (SimpleBarConfigs <$> MV.newMVar [])
      monitorNumbers <- useAllMonitors

      let lookupWithIndex barConfigs monitorNumber =
            (monitorNumber, lookup monitorNumber barConfigs)

          lookupAndUpdate barConfigs = do

            let (alreadyPresent, toCreate) =
                  partition (isJust . snd) $
                  map (lookupWithIndex barConfigs) monitorNumbers
                alreadyPresentConfigs = mapMaybe snd alreadyPresent

            newlyCreated <-
              mapM (forkM return myBarConfig . fst) toCreate
            let result = map snd newlyCreated ++ alreadyPresentConfigs
            return (barConfigs ++ newlyCreated, result)

      lift $ MV.modifyMVar configsVar lookupAndUpdate

cpuCfg = def
                 { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                 , graphLabel = Just "cpu"
                 }
clock = textClockNewWith def
cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
workspaces = workspacesNew def
simpleConfig = def
                       { startWidgets = [ workspaces ]
                       , endWidgets = [  clock, cpu ]
                       , barPosition = Bottom
                       , barPadding = 5
                       , barHeight = ExactSize 25
                       }

main = startTaffybar myTaffyConfig
