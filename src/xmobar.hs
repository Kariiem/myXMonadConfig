{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

  -- Builtin
import Text.Printf as P
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe

  -- Basic
import Xmobar

  -- MyLib
import Color.Theme

type Name = String
type Size = Int
data FontStyle = Normal | Bold | Italic | BoldItalic deriving (Eq,Ord)
instance Show FontStyle where
  show Normal = ""
  show Bold = "Bold"
  show Italic = "Italic"
  show BoldItalic = "Bold Italic"
data Font = Font Name FontStyle Size deriving (Eq,Ord,Show)

fontToString (Font name style i) = name ++ " " ++ show style ++ " " ++ show i

fontMap :: M.Map Font Int
fontMap = M.fromList $ zip
                [ Font name style size
                | name <-["Hack","JetBrains Mono"]
                , size <-[6..14]
                , style <- [Normal, Bold, Italic, BoldItalic] ]
                [1..]

class MyMonitor a where
  templateString::String
  monfont::Font
  monfont = Font "JetBrains Mono" Bold 9
  color::String
  color = "#0088ff"
  action::String
  action = ":"

  def :: [(String, String)]
  def = []

  monitorSpecific :: [(String, String)]
  monitorSpecific  = []

  toArgs ::  [String]
  toArgs = concatMap (\(x, y) -> [x, y]) ((def @a) ++ (monitorSpecific @a) )

  monitorTemplate :: String
  monitorTemplate = P.printf "<fn=%d><fc=%s><action=`%s`>%s</action></fc></fn>"
    templateFont (color @a) (action @a) barString
    where
      barString = templateString @a ++ "<hspace=5/><fc=#c2a2e4>\x2223</fc><hspace=5/>"
      templateFont :: Int
      templateFont = fromMaybe 1 (M.lookup (monfont @a) fontMap)

data MyBattery
battery = BatteryP ["BAT1"] (toArgs @MyBattery) 360

instance MyMonitor MyBattery where
  templateString =  "%battery%"
  color = "#ee33bb"
  def =
      [ ("-t", "<acstatus><left>%"),
        ("-L", "20"),
        ("-H", "80"),
        ("-p", "3")
      ]
  monitorSpecific =
      [ ("--", ""),
        ("-i", "<fc=#0088aa>Full</fc>"), -- idle AC, fully charged
        ("-O", "<fn=10>\x1f50c</fn>"), -- \xf583"   -- On AC, charging
        ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),  -- off AC, discharging
        ("-p", "green"),
        ("-A", "30"),
        ("-a", "status=$(cat /sys/class/power_supply/BAT1/status); [ \"$status\" = \"Discharging\" ] && notify-send -u critical \"Battery is running out!\" ")
      ]

data MyBrightness
brightness = Brightness (toArgs @MyBrightness) 1

instance MyMonitor MyBrightness where
  templateString = "%bright%"
  color = "#dfaa11"
  monfont = Font "Hack" BoldItalic 10
  def =
      [ ("-t", "<bar>"),
        ("-W", "10"),
        ("-b", " "), -- \x1fb8f
        ("-f", "\x1fb39")
      ]
  monitorSpecific =
      [ ("--", ""),
        ("-D", "intel_backlight"),
        ("-C", "actual_brightness"),
        ("-M", "max_brightness")
      ]

data MyCpu
cpu = MultiCpu (toArgs @MyCpu) 50

instance MyMonitor MyCpu where
  templateString = "cpu:%multicpu%"
  color = "#ff8855"
  action = "st -e btop"
  def =
      [ ("-t", "<total>%"),
        ("-L", "5"),
        ("-H", "50"),
        ("-l", "#ff8855"),
        ("-h", "red")
      ]

data MyTemp
temperature = MultiCoreTemp (toArgs @MyTemp) 50

instance MyMonitor MyTemp where
  templateString = "%multicoretemp%"
  color = "#ff647f"
  def =
      [ ("-t", "temp:<avg>\x2103"),
        ("-L", "60"),
        ("-H", "80"),
        ("-l", "#ff647f"),
        ("-h", "red")
      ]
  monitorSpecific =
      [ ("--" , ""),
        ("--mintemp","20"),
        ("--maxtemp","100")
      ]

data MyMemory
memory = Memory (toArgs @MyMemory) 20

instance MyMonitor MyMemory where
  templateString = "mem:%memory%"
  color = "#ff6600"
  action = "st -e btop"
  def = [("-t", "<used>mb(<usedratio>%)")]

data MyVolume
volume = Alsa "default" "Master" (toArgs @MyVolume)

instance MyMonitor MyVolume where
  templateString = "%alsa:default:Master%"
  def =
    [("-t", "Vol: <volume>% <status>")]
  monitorSpecific =
      [ ("--", "")
      , ("-C", "#00ff00")
      ]

data MyUpdates
updates = Com "/bin/bash" (toArgs @MyUpdates) "updates" 36000
instance MyMonitor MyUpdates where
  templateString = "\xf0f3 %updates% updates"
  action = "$XDG_CONFIG_HOME/scripts/yad/update"
  color = "#ff0000"
  monitorSpecific = [("-c", "{ checkupdates ; aur -Qua; } | wc -l")]

data MyTrayer
trayer = XPropertyLog "_XMONAD_STRAYPAD"
instance MyMonitor MyTrayer where
  templateString = "%_XMONAD_STRAYPAD%"

data MyDate
datetime = Date (concat $ toArgs @MyDate) "date" 20

instance MyMonitor MyDate where
  templateString = "%date%"
  color = "#40a5ff"
  toArgs = ["%I:%M %b %d %Y"]

data MyKbd
kbd = Kbd [("us","US"),("ara","AR")]

instance MyMonitor MyKbd where
  templateString = "%kbd%"
  color = "#ff85aa"

main :: IO ()
main = xmobar config

config :: Config
config =
  defaultConfig
    {
      lowerOnStart = True,
      overrideRedirect = True,
      template =
        " <icon=haskell.xpm/> %UnsafeXMonadLog% }{"
        ++ monitorTemplate @MyUpdates
        ++ monitorTemplate @MyVolume
--        ++ monitorTemplate @MyBrightness
        ++ monitorTemplate @MyTemp
        ++ monitorTemplate @MyMemory
        ++ monitorTemplate @MyCpu
        ++ monitorTemplate @MyDate
        ++ monitorTemplate @MyKbd
        ++ monitorTemplate @MyBattery
        ++ monitorTemplate @MyTrayer ,

      font = "JetBrains Mono 9" --"Hack Bold Italic 8"
    ,
      position = TopHM 25 10 10 5 5, -- Height, left/right margins, top/down margins
      additionalFonts = map (fontToString.fst) $ L.sortOn snd $ M.toList fontMap ,--map snd fontList,
      allDesktops = True,
      alpha = 255,
      bgColor = colorBack theme,
      fgColor = colorFore theme,
      iconRoot = "/home/karim/.config/xmonad/icons",
      commands =
        [ Run UnsafeXMonadLog
        , Run memory
        , Run kbd
--        , Run brightness
        , Run battery
        , Run datetime
        , Run trayer
        , Run cpu
        , Run temperature
        , Run updates
        , Run volume
        ],
      alignSep = "}{",
      sepChar  = "%"
    }
