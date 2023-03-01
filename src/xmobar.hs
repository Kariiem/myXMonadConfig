  -- Builtin
import Text.Printf as P

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

action command str = "<action=`"++command ++ "`>" ++ str ++"</action>"

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
  def :: a -> [(String, String)]
  def = const []

  monitorSpecific :: a -> [(String, String)]
  monitorSpecific  = const []

  monitorTemplate :: a -> String
  monitorTemplate = const ""

  toArgs :: a -> [String]
  toArgs a = concatMap (\(x, y) -> [x, y]) (def a ++ monitorSpecific a)

data MyBattery = MyBattery
battery = BatteryP ["BAT1"] (toArgs MyBattery) 360

instance MyMonitor MyBattery where
  def =
    const
      [ ("-t", "<acstatus><left>%"),
        ("-L", "20"),
        ("-H", "80"),
        ("-p", "3")
      ]
  monitorSpecific =
    const
      [ ("--", ""),
        ("-i", "<fc=#0088aa>Full</fc>"), -- idle AC, fully charged
        ("-O", "\x1F50C"), -- \xf583"   -- On AC, charging
        ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),  -- off AC, discharging
        ("-p", "green"),
        ("-A", "30"),
        ("-a", "notify-send -u critical 'Battery is running out!'")
      ]
  monitorTemplate = const
                  $ fontSize 6
                  $ colorize "#ee33bb"
                  $ dtBox "%battery%"  "Bottom" "#ee33bb"

data MyBrightness = MyBrightness
brightness = Brightness (toArgs MyBrightness) 1

instance MyMonitor MyBrightness where
  def =
    const
      [ ("-t", "<bar>"),
        ("-W", "10"),
        ("-b", " "), -- \x1fb8f
        ("-f", "\x1fb39") 
      ]
  monitorSpecific =
    const
      [ ("--", ""),
        ("-D", "intel_backlight"),
        ("-C", "actual_brightness"),
        ("-M", "max_brightness")
      ]
  monitorTemplate = const
                  $ fontSize 6
                  $ colorize "#dfaa11" -- "#fffa55"
                  $ dtBox "%bright%" "Bottom" "#dfaa11" -- <fn=1>\x1f317</fn>

data MyCpu = MyCpu
cpu = MultiCpu (toArgs MyCpu) 50
instance MyMonitor MyCpu where
  def =
    const
      [ ("-t", "<total>%"),
        ("-L", "5"),
        ("-H", "50"),
        ("-l", "#ff8855"),
        ("-h", "red")
      ]
  monitorTemplate = const
                  $ action "st -e btop"
                  $ fontSize 6
                  $ colorize "#ff8855"
                  $ dtBox "<fn=1>\xf26c</fn>  cpu:%multicpu%" "Bottom" "#ff8855"

data MyTemp = MyTemp
temperature = MultiCoreTemp (toArgs MyTemp) 50
instance MyMonitor MyTemp where
  def =
    const
      [ ("-t", "temp:<avg>\x2103"),
        ("-L", "60"),
        ("-H", "80"),
        ("-l", "#ff647f"),
        ("-h", "red")
      ]
  monitorSpecific =
    const
      [ ("--" , ""),
        ("--mintemp","20"),
        ("--maxtemp","100")
      ]
  monitorTemplate = const
                  $ fontSize 6
                  $ colorize "#ff647f"
                  $ dtBox "%multicoretemp%" "Bottom" "#ff647f"

data MyMemory = MyMemory
memory = Memory (toArgs MyMemory) 20

instance MyMonitor MyMemory where
  def = const [("-t", "<used>mb(<usedratio>%)")]
  monitorTemplate = const
                  $ fontSize 6
                  $ colorize "#ff6600"
                  $ dtBox "mem:%memory%" "Bottom" "#ff6600"

checkUpdates = Com "/bin/bash" ["-c","{ checkupdates ; yay -Qua; } | wc -l"] "updates" 36000
checkUpdatesTemplate = action "$XDG_CONFIG_HOME/scripts/yad/yad_update"
                $ fontSize 6
                $ colorize "#ff0000"
                $ dtBox "\xf0f3 %updates% updates" "Bottom" "#ff0000"

trayer = Com "/bin/bash" [ "-c", "$XDG_CONFIG_HOME/xmonad/icon_padding" ] "trayerpad" 10
trayerTemplate = "%trayerpad%"

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
      template =
        " <icon=haskell.xpm/> %UnsafeXMonadLog% }{"
        ++ checkUpdatesTemplate
        ++ monitorTemplate MyBrightness
        ++ monitorTemplate MyTemp
        ++ monitorTemplate MyMemory
        ++ monitorTemplate MyCpu
        ++ dateTemplate
        ++ kbdTemplate
        ++ monitorTemplate MyBattery
        ++ trayerTemplate,

      font = "Hack Bold Italic 12",
      position = TopHM 30 10 10 5 5, -- Height, left/right margins, top/down margins
      additionalFonts = map snd fontList,
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
