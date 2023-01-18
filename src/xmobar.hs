import Text.Printf as P
import Theme
import Xmobar

main :: IO ()
main = xmobar config

config :: Config
config =
  defaultConfig
    { font = "Hack 12", -- "Noto Color Emoji 10"
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
      template =
        " <icon=haskell.xpm/> %UnsafeXMonadLog% }{ \
        \ %battery% | %bright% | %kbd% | %date% "
          ++ box "%memory%" "Bottom" (colorBRed theme) 3 (0, 2, 0, 0)
          ++ " %trayerpad% ",
      alignSep = "}{",
      sepChar  = "%"
    }

trayer = Com "/bin/sh" [ "-c", "$XDG_CONFIG_HOME/xmonad/scripts/icon_padding" ] "trayerpad" 10
-- trayer = Com "$XDG_CONFIG_HOME/xmonad/scripts/icon_padding.sh" [] "trayerpad" 20

kbd = Kbd []

brightness = Brightness (toArgs MyBrightness) 10

memory = Memory (toArgs MyMemory) 20

battery = BatteryP ["BAT1"] (toArgs MyBattery) 360

datetime = Date "%a %_d %b %Y <fc=#ee9a00>%I:%M</fc>" "date" 10

-----------------------------------------------------------
-- Commands
-----------------------------------------------------------
data MyBattery = MyBattery

data MyBrightness = MyBrightness

data MyMemory = MyMemory

class MyMonitor a where
  def :: a -> [(String, String)]
  def _ = []

  monitorSpecific :: a -> [(String, String)]
  monitorSpecific _ = []

  toArgs :: a -> [String]
  toArgs a = concatMap (\(x, y) -> [x, y]) (def a ++ monitorSpecific a)

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
        ("-i", "<fc=#0088aa><fn=1>\xf583</fn></fc>"),
        ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),
        ("-p", "green"),
        ("-A", "30"),
        ("-a", "notify-send -u critical 'Battery is running out!'")
      ]

instance MyMonitor MyBrightness where
  def = const [("-t", "<bar>")]
  monitorSpecific =
    const
      [ ("--", ""),
        ("-D", "intel_backlight"),
        ("-C", "actual_brightness"),
        ("-M", "max_brightness")
      ]

instance MyMonitor MyMemory where
  def = const [("-t", "mem: \xf233 <used>Mb (<usedratio>%)")]
  monitorSpecific = const []

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------
box :: String -> String -> String -> Int -> (Int, Int, Int, Int) -> String
box str ty color width (mt, mb, ml, mr) =
  P.printf "<box type=%s width=%d mt=%d mb=%d ml=%d mr=%d color=%s > %s </box> " ty width mt mb ml mr color str

-----------------------------------------------------------
