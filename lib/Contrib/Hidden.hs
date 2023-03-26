{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Contrib.Hidden where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet qualified as W
import XMonad.Util.Dmenu
import XMonad.Util.NamedWindows
import Data.Char
import Text.Printf
import Data.List
--------------------------------------------------------------------------------
newtype HiddenWindows a = HiddenWindows [Window] deriving (Show, Read)

--------------------------------------------------------------------------------

-- | Messages for the @HiddenWindows@ layout modifier.
data HiddenMsg
  = -- | Hide a window.
    HideWindow Window
  | -- | Restore window (FILO).
    PopNewestHiddenWindow
  | -- | Restore window (FIFO).
    PopOldestHiddenWindow
  | -- | Restore specific window.
    PopSpecificHiddenWindow Window
  | GetHidden
  deriving (Eq)

instance Message HiddenMsg

--------------------------------------------------------------------------------
instance LayoutModifier HiddenWindows Window where
  handleMess h@(HiddenWindows hidden) mess
    | Just (HideWindow win) <- fromMessage mess = hideWindowMsg h win
    | Just PopNewestHiddenWindow <- fromMessage mess = popNewestMsg h
    | Just PopOldestHiddenWindow <- fromMessage mess = popOldestMsg h
    | Just (PopSpecificHiddenWindow win) <- fromMessage mess = popSpecificMsg win h
    | Just ReleaseResources <- fromMessage mess = doUnhook
    | Just GetHidden <- fromMessage mess = getHiddenWinMsg h
    | otherwise = return Nothing
    where
      doUnhook = do
        mapM_ restoreWindow hidden
        return Nothing

  modifierDescription _ = "Hidden"

--------------------------------------------------------------------------------

-- | Apply the @HiddenWindows@ layout modifier.
hiddenWindows :: l Window -> ModifiedLayout HiddenWindows l Window
hiddenWindows = ModifiedLayout $ HiddenWindows []

--------------------------------------------------------------------------------

-- | Remove the given window from the current layout.  It is placed in
-- list of hidden windows so it can be restored later.
hideWindow :: Window -> X ()
hideWindow = sendMessage . HideWindow

--------------------------------------------------------------------------------

-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FIFO queue.  That is, the
-- first window hidden will be restored.
popOldestHiddenWindow :: X ()
popOldestHiddenWindow = sendMessage PopOldestHiddenWindow

--------------------------------------------------------------------------------

-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FILO queue.  That is, the
-- most recently hidden window will be restored.
popNewestHiddenWindow :: X ()
popNewestHiddenWindow = sendMessage PopNewestHiddenWindow

popHiddenWindow :: Window -> X ()
popHiddenWindow = sendMessage . PopSpecificHiddenWindow

--------------------------------------------------------------------------------
hideWindowMsg :: HiddenWindows a -> Window -> X (Maybe (HiddenWindows a))
hideWindowMsg (HiddenWindows hidden) win = do
  modify (\s -> s {windowset = W.delete' win $ windowset s})
  return . Just . HiddenWindows $ hidden ++ [win]

--------------------------------------------------------------------------------
popNewestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popNewestMsg (HiddenWindows []) = return Nothing
popNewestMsg (HiddenWindows hidden) = do
  let (win, rest) = (last hidden, init hidden)
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popOldestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popOldestMsg (HiddenWindows []) = return Nothing
popOldestMsg (HiddenWindows (win : rest)) = do
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popSpecificMsg :: Window -> HiddenWindows a -> X (Maybe (HiddenWindows a))
popSpecificMsg _ (HiddenWindows []) = return Nothing
popSpecificMsg win (HiddenWindows hiddenWins) =
  if win `elem` hiddenWins
    then do
      restoreWindow win
      return . Just . HiddenWindows $ filter (/= win) hiddenWins
    else return . Just . HiddenWindows $ hiddenWins

--------------------------------------------------------------------------------
restoreWindow :: Window -> X ()
restoreWindow = windows . W.insertUp

--------------------------------------------------------------------------------
getHiddenWindows :: X ()
getHiddenWindows = sendMessage GetHidden

--------------------------------------------------------------------------------
getHiddenWinMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
getHiddenWinMsg h@(HiddenWindows []) =do
  io (spawn "notify-send -t 400 \"There are no hidden windows\"")
  return $ Just h
getHiddenWinMsg h@(HiddenWindows hidden) = do
  winTitles <- mapM getName hidden
  winNames <- mapM getNameWMClass  hidden
  win <- io.myDmenu.map head . group . sort
                      $ [printf "%-60s %-s" ("["++ show wid ++"]")
                                            (show  wName++": " ++ show wTitle)
                      | wTitle<- winTitles
                      , wName<- winNames
                      , wid<- hidden]
  let winId:_ = read (takeWhile (not.isSpace) win):: [Window]
  popSpecificMsg winId h
    where
     myDmenu = menuArgs "dmenu" ["-z","1910","-l","10","-p","select window: "]
