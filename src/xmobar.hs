import Text.Printf as P
import Xmobar

main :: IO ()
main = xmobar config

box :: String ->String -> String -> Int -> (Int, Int, Int, Int) -> String
box str ty color width (mt, mb, ml, mr) =
  P.printf
    "<box type=%s width=%d mt=%d mb=%d ml=%d mr=%d color=%s > %s </box>"
    ty
    width
    mt
    mb
    ml
    mr
    color
    str

config :: Config
config =
  defaultConfig
    { font = "Hack 12", -- "Noto Color Emoji 10"
      position = TopHM 30 10 10 5 5, -- TopSize C 98 30
      additionalFonts = ["Hack 18"],
      allDesktops = True,
      alpha = 200,
      bgColor = black,
      iconRoot = "/home/karim/.config/xmonad/",
      commands =
        [ Run UnsafeXMonadLog,
          Run memory,
          Run $ Kbd [],
          Run brightness,
          Run battery,
          Run datetime
        ],
      template =
        " <icon=haskell.xpm/> %UnsafeXMonadLog%  <fn=1>\xf17c</fn>}{ \
        \%default:Master% | %battery% | %bright% | %kbd% | %date% "
          ++ box "%memory%" "VBoth" "red" 2 (0, 5, 0, 0), alignSep = "}{"
    }

black = "#223344"

class MyMonitor a where
  def :: a -> [(String, String)]
  def _ = []

  monitorSpecific :: a -> [(String, String)]
  monitorSpecific _ = []

  toArgs :: a -> [String]
  toArgs a = concatMap (\(x, y) -> [x, y]) (def a ++ monitorSpecific a)

data MyBattery = MyBattery

data MyBrightness = MyBrightness

data MyMemory = MyMemory

instance MyMonitor MyBattery where
  def _ =
    [ ("-t", "<acstatus> <left>%"),
      ("-L", "20"),
      ("-H", "80"),
      ("-p", "3")
    ]
  monitorSpecific _ =
    [ ("--", ""),
      ("-O", "<fc=#0088aa>On</fc>"),
      ("-i", "<fc=#0088aa><fn=1>\xf583</fn></fc>"),
      ("-o", "<fc=#33aa55><fn=1>\xf242 </fn></fc>"),
      ("-p", "green"),
      ("-A", "30"),
      ("-a", "notify-send -u critical 'Battery is running out!'")
    ]

instance MyMonitor MyBrightness where
  def _ = [("-t", "<bar>")]
  monitorSpecific _ =
    [ ("--", ""),
      ("-D", "intel_backlight"),
      ("-C", "actual_brightness"),
      ("-M", "max_brightness")
    ]

instance MyMonitor MyMemory where
  def _ = [("-t", "mem: \xf233 <used>Mb (<usedratio>%)")]
  monitorSpecific _ = []

brightness = Brightness (toArgs MyBrightness) 10

memory = Memory (toArgs MyMemory) 20

battery = BatteryP ["BAT1"] (toArgs MyBattery) 360

datetime = Date "%a %_d %b %Y <fc=#ee9a00>%I:%M</fc>" "date" 10
