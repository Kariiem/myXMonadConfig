import Xmobar

main :: IO ()
main = xmobar config

config :: Config
config =
  defaultConfig
    { font = "Hack 12" -- "Noto Color Emoji 10"
 
    , position = TopHM 30 10 10 5 5 -- TopSize C 98 30
    , additionalFonts =
       [ 
       "Hack 14"
       ]
    , allDesktops = True
    , alpha = 255
    , bgColor = black -- "#ffffff" -- black
    , iconRoot = "/home/karim/.config/xmonad/"
    , commands =
        [ Run UnsafeXMonadLog
        , Run memory
        , Run $ Kbd []
	, Run $ Volume "default" "Master" [] 10
        , Run brightness
        , Run battery
        , Run $ Date "%a %_d %b %Y <fc=#ee9a00>%I:%M</fc>" "date" 10
        ]
    , template = " <icon=haskell.xpm/> %UnsafeXMonadLog%  \xf583}{ %default:Master% | %battery% | %bright% | %kbd% | %date% | %memory%"
    , alignSep = "}{"
    }

dwmBlue = "#2b4f98"
black="#223344"
brightness = Brightness ["--", "-D", "intel_backlight"] 10
memory = Memory ["-t", "Mem: <used>mb "] 10
battery =
  BatteryP
    ["BAT1"]
    [ "-t"
    , "<acstatus><watts> (<left>%)"
    , "-L"
    , "10"
    , "-H"
    , "80"
    , "-p"
    , "3"
    , "--"
    , "-O"
    , "<fc=green>On</fc> - "
    , "-i"
    , ""
    , "-L"
    , "-15"
    , "-H"
    , "-5"
    , "-l"
    , "red"
    , "-m"
    , "blue"
    , "-h"
    , "green"
    , "-a"
    , "notify-send -u critical 'Battery running out!!'"
    , "-A"
    , "3"
    ]
    600
    

