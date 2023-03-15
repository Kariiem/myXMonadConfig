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

box :: String -> String -> String -> Int -> (Int, Int, Int, Int) -> String
box str ty color width (mt, mb, ml, mr) =
  P.printf "<box type=%s width=%d mt=%d mb=%d ml=%d mr=%d color=%s > %s </box> " ty width mt mb ml mr color str

dtBox str ty color = box str ty color 3 (0,0,0,0)

colorize ::String -> String ->String
colorize color str = "<fc=" ++ color ++ ">" ++ str ++"</fc>"

fontSize :: Int->String->String
fontSize i str = "<fn="++show i++ ">"++ str ++ "</fn>"

action command str = "<action=`"++ command ++ "`>" ++ str ++"</action>"

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
                | name <-["Hack"]
                , size <-[8..14]
                , style <- [Normal, Bold, Italic, BoldItalic] ]
                [1..]
fontList = [ (1,"Hack Bold 14")
           , (2,"Hack Bold Italic 14")
           , (3,"Hack Bold 12")
           , (4,"Hack Bold Italic 12")
           , (5,"Hack Bold 10")
           , (6,"Hack Bold Italic 10")
           , (7,"Hack Bold 8")
           , (8,"Hack Bold Italic 8")
           , (9,"Hack 8")
           ]

class MyMonitor a where
  templateString::String
  monfont::Font
  monfont = Font "Hack" BoldItalic 8
  color::String
  color = "#0088aa"
  monaction::String
  monaction = ":"

  def :: [(String, String)]
  def = []

  monitorSpecific :: [(String, String)]
  monitorSpecific  = []

  toArgs ::  [String]
  toArgs = concatMap (\(x, y) -> [x, y]) ((def @a) ++ (monitorSpecific @a) )

  monitorTemplate :: String
  monitorTemplate = P.printf "<action=`%s`><fn=%d><fc=%s>%s</fc></fn></action>"
    (monaction @a)  templateFont (color @a)  (dtBox (templateString @a) "Bottom" (color @a) )
    where
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
        ("-O", "\x1F50C"), -- \xf583"   -- On AC, charging
        ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),  -- off AC, discharging
        ("-p", "green"),
        ("-A", "30"),
        ("-a", "notify-send -u critical 'Battery is running out!'")
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
  monaction = "st -e btop"
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
  monaction = "st -e btop"
  def = [("-t", "<used>mb(<usedratio>%)")]

checkUpdates = Com "/bin/bash" ["-c","{ checkupdates ; yay -Qua; } | wc -l"] "updates" 36000
checkUpdatesTemplate = action "$XDG_CONFIG_HOME/scripts/yad/update"
                $ fontSize 6
                $ colorize "#ff0000"
                $ dtBox "\xf0f3 %updates% updates" "Bottom" "#ff0000"

trayer = XPropertyLog "_XMONAD_STRAYPAD"
trayerTemplate = "%_XMONAD_STRAYPAD%"

datetime = Date "<fc=#00d5c8>%I:%M</fc> %b %d %Y" "date" 10
dateTemplate = fontSize 6
             $ colorize "#e0a5ff"
             $ dtBox "<fn=5>\x1f551</fn> %date%" "Bottom" "#e0a5ff"

kbd = Kbd []
kbdTemplate = colorize "#ff85ac"
            $ fontSize 6
            $ dtBox "\x2328 %kbd%" "Bottom" "#ff85ac"

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
        ++ checkUpdatesTemplate
        ++ monitorTemplate @MyBrightness
        ++ monitorTemplate @MyTemp
        ++ monitorTemplate @MyMemory
        ++ monitorTemplate @MyCpu
        ++ dateTemplate
        ++ kbdTemplate
        ++ monitorTemplate @MyBattery
        ++ trayerTemplate ,

      font = "Hack Bold Italic 8",
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
        , Run brightness
        , Run battery
        , Run datetime
        , Run trayer
        , Run cpu
        , Run temperature
        , Run checkUpdates
        ],
      alignSep = "}{",
      sepChar  = "%"
    }
