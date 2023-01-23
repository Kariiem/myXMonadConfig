module Core.MyScratchpads where

import XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

scratchpads =
  [ -- run htop in xterm, find it by title, use default floating window placement
    NS "notes" "st -e nvim" (title =? "notes") defaultFloating,
    NS
      "stardict"
      "stardict"
      (className =? "Stardict")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]
