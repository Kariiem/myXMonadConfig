  -- Builtin
import Text.Printf as P

  -- Basic
import Xmobar

  -- MyLib
import Color.Theme

box :: String -> String -> String -> Int -> (Int, Int, Int, Int) -> String
box str ty color width (mt, mb, ml, mr) =
  P.printf "<box type=%s width=%d mt=%d mb=%d ml=%d mr=%d color=%s > %s </box> " ty width mt mb ml mr color str

colorize ::String -> String ->String
colorize color str = "<fc=" ++ color ++ ">" ++ str ++"</fc>"

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
      [ ("-t", "<acstatus> <left>%"),
        ("-L", "20"),
        ("-H", "80"),
        ("-p", "3")
      ]
  monitorSpecific =
    const
      [ ("--", ""),
        ("-O", "<fc=#0088aa>On</fc>"),
        ("-i", "<fc=#0088aa><fn=1>\xf492</fn></fc>"), -- \xf583
        ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),
        ("-p", "green"),
        ("-A", "30"),
        ("-a", "notify-send -u critical 'Battery is running out!'")
      ]
  monitorTemplate = const $ colorize (colorYellow theme) $ box "%battery%"  "Bottom" (colorYellow theme) 3 (0,2,0,0)

data MyBrightness = MyBrightness
brightness = Brightness (toArgs MyBrightness) 10

instance MyMonitor MyBrightness where
  def = const
          [ ("-t", "<bar>"),
            ("-W", "8"),
            ("-b", "\x1fb8f"),
            ("-f", "\x1fb39")
          ]
  monitorSpecific =
    const
      [ ("--", ""),
        ("-D", "intel_backlight"),
        ("-C", "actual_brightness"),
        ("-M", "max_brightness")
      ]
  monitorTemplate = const $ "<fc=#ffffff><icon=brightness.xpm/></fc>" ++ (colorize (colorGreen theme) $ box "%bright%" "Bottom" (colorBPurple theme) 3 (0,2,0,0))

data MyMemory = MyMemory
memory = Memory (toArgs MyMemory) 20

instance MyMonitor MyMemory where
  def = const [("-t", "mem: \xf233 <used>Mb (<usedratio>%)")]
  monitorTemplate = const $ colorize (colorCyan theme) $ box "%memory%" "Bottom" (colorPurple theme) 3 (0, 2, 0, 0)

trayer = Com "/bin/sh" [ "-c", "$XDG_CONFIG_HOME/xmonad/scripts/icon_padding" ] "trayerpad" 10
trayerTemplate = "%trayerpad%"

datetime = Date "<fc=#ff6608>%a</fc> %_d/%-m/%Y <fc=#30d5c8>%I:%M</fc> %P" "date" 10
dateTemplate = colorize (colorBGreen theme) $ box "%date%" "Bottom" (colorBRed theme) 3 (0,2,0,0)

kbd = Kbd []
kbdTemplate = colorize (colorYellow theme) $ box "%kbd%" "Bottom" (colorOrange theme) 3 (0,2,0,0)

main :: IO ()
main = xmobar config

config :: Config
config =
  defaultConfig
    {
      template =
        " <icon=haskell.xpm/> %UnsafeXMonadLog% }{"
        ++ monitorTemplate MyBrightness
        ++ monitorTemplate MyMemory
        ++ kbdTemplate
        ++ monitorTemplate MyBattery
        ++ dateTemplate
        ++ trayerTemplate,

      font = "Hack 12", -- "Noto Color Emoji 10"
      position = TopHM 30 10 10 5 5, -- Height, left/right margins, top/down margins
      additionalFonts = ["Hack 18"],
      allDesktops = True,
      alpha = 255,
      bgColor = colorBack theme,
      fgColor = colorFore theme,
      iconRoot = "/home/karim/.config/xmonad/icons",
      commands =
        [ Run UnsafeXMonadLog,
          Run memory,
          Run kbd,
          Run brightness,
          Run battery,
          Run datetime,
          Run trayer
       ],
      alignSep = "}{",
      sepChar  = "%"
    }
