{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module MyMaxMin where
import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as S
import XMonad.Prelude


    
data Maximize a = Maximize Dimension (Maybe Window) deriving ( Read, Show )
maximize :: LayoutClass l Window => l Window -> ModifiedLayout Maximize l Window
maximize = ModifiedLayout $ Maximize 25 Nothing

-- | Like 'maximize', but allows you to specify the amount of padding
-- placed around the maximized window.
maximizeWithPadding :: LayoutClass l Window => Dimension -> l Window -> ModifiedLayout Maximize l Window
maximizeWithPadding padding = ModifiedLayout $ Maximize padding Nothing

newtype MaximizeRestore = MaximizeRestore Window deriving ( Eq )
instance Message MaximizeRestore
maximizeRestore :: Window -> MaximizeRestore
maximizeRestore = MaximizeRestore

instance LayoutModifier Maximize Window where
    modifierDescription (Maximize _ _) = "Maximize"
    pureModifier (Maximize padding (Just target)) rect (Just (S.Stack focused _ _)) wrs =
            if focused == target
                then (maxed ++ rest, Nothing)
                else (rest ++ maxed, lay)
        where
            (toMax, rest) = partition (\(w, _) -> w == target) wrs
            maxed = map (\(w, _) -> (w, maxRect)) toMax
            maxRect = Rectangle (rect_x rect + fromIntegral padding)
                                (rect_y rect + fromIntegral padding)
                                (rect_width rect  - padding * 2)
                                (rect_height rect - padding * 2)
            lay | null maxed = Just (Maximize padding Nothing)
                | otherwise  = Nothing
    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (Maximize padding mw) m = case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just w' -> if w == w'
                        then Just $ Maximize padding Nothing   -- restore window
                        else Just $ Maximize padding $ Just w  -- maximize different window
            Nothing -> Just $ Maximize padding $ Just w        -- maximize window
        _ -> Nothing
