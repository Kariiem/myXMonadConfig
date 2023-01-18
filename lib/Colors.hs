module Colors where

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
  colorWhite = const "#e2e2dc"

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

  colorBlack = const "#1E2029"
  colorGrey = const "#3f444a"
  colorWhite = const "#e2e2dc"

  colorBlue = const "#2257A0"
  colorBBlue = const "#51afef"

  colorCyan = const "#5699af"
  colorBCyan = const "#46D9FF"

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

  colorBlack = const "#191C25"
  colorGrey = const "#434C5E"
  colorWhite = const "#F0F4FC"

  colorGreen = const "#a3be8c"
  colorBGreen = const "#8FBCBB"

  colorBlue = const "#5E81AC"
  colorBBlue = const "#81A1C1"

  colorCyan = const "#507681"
  colorBCyan = const "#88C0D0"

  colorRed = const "#bf616a"
  colorBRed = const "#D08770"

  colorPurple = const "#b48ead"
  colorBPurple = const "#5d80ae"

  colorOrange = const "#d08770"
  colorYellow = const "#ebcb8b"

instance Color NordLight where
  colorBack = const "#E5E9F0"
  colorFore = const "#3B4252"

  colorBlack = const "#60728C"
  colorGrey = const "#B8C5DB"
  colorWhite = const "#485163"

  colorBlue = const "#5272AF"
  colorBBlue = const "#3B6EA8"

  colorCyan = const "#2C7088"
  colorBCyan = const "#398EAC"

  colorGreen = const "#4F894C"
  colorBGreen = const "#29838D"

  colorRed = const "#99324B"
  colorBRed = const "#AC4426"

  colorPurple = const "#97365B"
  colorBPurple = const "#842879"

  colorOrange = const "#AC4426"
  colorYellow = const "#9A7500"
