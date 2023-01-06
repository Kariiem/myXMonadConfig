import Xmobar

main :: IO ()
main = xmobar config

config :: Config
config =
  defaultConfig
    { font = "xft:Fira Code:style=medium:pixelsize=16:antialias=true:hinting=true"
    , position = TopSize L 100 24
    , -- , additionalFonts =
      -- [ "xft:Symbola-9"
      -- , "xft:Symbola-10"
      -- , "xft:Symbola-11"
      -- , "xft:Symbola-11"
      -- , "xft:Symbola-12"
      -- , "xft:FontAwesome-10"
      -- , "xft:FontAwesome-9"
      -- ]
      allDesktops = True
    , alpha = 255
    , bgColor = black
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
    , template = " <icon=haskell.xpm/> %UnsafeXMonadLog%  }{ %default:Master% | %battery% | %bright% | %kbd% | %date% | %memory%"
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
