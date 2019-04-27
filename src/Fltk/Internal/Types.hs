-- | All the types, to avoid cyclic imports.

{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Fltk.Internal.Types where

import Fltk.Internal.Upcast (Upcast(..))

import Data.Coerce (coerce)

import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


-- | <https://www.fltk.org/doc-1.4/classFl__Box.html>
newtype Box
  = Box { unBox :: Fltk.Ref Fltk.Box }


-- | <https://www.fltk.org/doc-1.4/classFl__Button.html>
newtype Button
  = Button { unButton :: Fltk.Ref Fltk.ButtonBase }

instance Upcast Button Widget where upcast = coerce


-- | <https://www.fltk.org/doc-1.4/classFl__Double__Window.html>
newtype DoubleWindow
  = DoubleWindow { unDoubleWindow :: Fltk.Ref Fltk.DoubleWindowBase }

instance Upcast DoubleWindow Group  where upcast = coerce
instance Upcast DoubleWindow Widget where upcast = coerce
instance Upcast DoubleWindow Window where upcast = coerce


-- | A 'Group' is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Group.html>
newtype Group
  = Group { unGroup :: Fltk.Ref Fltk.GroupBase }

instance Upcast Group Widget where upcast = coerce


-- | <https://www.fltk.org/doc-1.4/classFl__Image.html>
newtype Image
  = Image { unImage :: Fltk.Ref Fltk.Image }


-- | <https://www.fltk.org/doc-1.4/classFl__Input.html>
newtype Input
  = Input { unInput :: Fltk.Ref Fltk.InputBase }

instance Upcast Input Widget where upcast = coerce


data LabelStyle
  = LabelStyle Fltk.Color Fltk.Font Fltk.FontSize Fltk.Labeltype


-- | <https://www.fltk.org/doc-1.4/structFl__Menu__Item.html>
newtype MenuItem
  = MenuItem { unMenuItem :: Fltk.Ref Fltk.MenuItemBase }


-- | <https://www.fltk.org/doc-1.4/classFl__Widget.html>
newtype Widget
  = Widget { unWidget :: Fltk.Ref Fltk.WidgetBase }


-- | <https://www.fltk.org/doc-1.4/classFl__Window.html>
newtype Window
  = Window { unWindow :: Fltk.Ref Fltk.WindowBase }

instance Upcast Window Widget where upcast = coerce
