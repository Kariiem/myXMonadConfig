module Color.MyPalette where
class Color a where
  colorBack,
    colorFore,
    colorBlack,
    colorGrey,
    colorWhite,
    colorBlue,
    colorBBlue,
    colorCyan,
    colorBCyan,
    colorGreen,
    colorBGreen,
    colorRed,
    colorBRed,
    colorPurple,
    colorBPurple,
    colorOrange,
    colorYellow ::
      a -> String

data Dracula = Dracula

data DoomOne = DoomOne

data Nord = Nord

data NordLight = NordLight

instance Color Dracula where
  colorBack = const "#282a36"
  colorFore = const "#f8f8f2"

  colorBlack = const "#1E2029"
  colorGrey = const "#565761"
  colorWhite = const "#f8f8f2"

  colorBlue = const "#0189cc"
  colorBBlue = const "#61bfff"

  colorCyan = const "#8be9fd"
  colorBCyan = const "#8be9fd"

  colorGreen = const "#50fa7b"
  colorBGreen = const "#0189cc"

  colorRed = const "#ff5555"
  colorBRed = const "#ffb86c"

  colorPurple = const "#ff79c6"
  colorBPurple = const "#bd93f9"

  colorOrange = const "#ffb86c"
  colorYellow = const "#f1fa8c"

instance Color DoomOne where
  colorBack = const "#282c34"
  colorFore = const "#bbc2cf"

  colorBlack = const "#1b2229"
  colorGrey = const "#3f444a"
  colorWhite = const "dfdfdf"

  colorBlue = const "#2257a0"
  colorBBlue = const "#51afef"

  colorCyan = const "#5699af"
  colorBCyan = const "#46d9ff"

  colorGreen = const "#98be65"
  colorBGreen = const "#4db5bd"

  colorRed = const "#ff6c6b"
  colorBRed = const "#da8548"

  colorPurple = const "#a9a1e1"
  colorBPurple = const "#c678dd"

  colorOrange = const "#da8548"
  colorYellow = const "#ecbe7b"

instance Color Nord where
  colorBack = const "#2e3440"
  colorFore = const "#eceff4"

  colorBlack = const "#191c25"
  colorGrey = const "#434c5e"
  colorWhite = const "#f0f4fc"

  colorGreen = const "#a3be8c"
  colorBGreen = const "#8fbcbb"

  colorBlue = const "#5e81ac"
  colorBBlue = const "#81a1c1"

  colorCyan = const "#507681"
  colorBCyan = const "#88c0d0"

  colorRed = const "#bf616a"
  colorBRed = const "#d08770"

  colorPurple = const "#b48ead"
  colorBPurple = const "#5d80ae"

  colorOrange = const "#d08770"
  colorYellow = const "#ebcb8b"

instance Color NordLight where
  colorBack = const "#e5e9f0"
  colorFore = const "#3b4252"

  colorBlack = const "#f0f4fc"
  colorGrey = const "#b8c5db"
  colorWhite = const "#485163"

  colorBlue = const "#5272af"
  colorBBlue = const "#3b6ea8"

  colorCyan = const "#2c7088"
  colorBCyan = const "#398eac"

  colorGreen = const "#4f894c"
  colorBGreen = const "#29838d"

  colorRed = const "#99324b"
  colorBRed = const "#ac4426"

  colorPurple = const "#97365B"
  colorBPurple = const "#842879"

  colorOrange = const "#ac4426"
  colorYellow = const "#9a7500"
