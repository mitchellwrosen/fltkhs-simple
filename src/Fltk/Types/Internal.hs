-- | All the types, to avoid cyclic imports.

{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Fltk.Types.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk


newtype Box
  = Box { unBox :: Fltk.Ref Fltk.Box }

newtype Group
  = Group { unGroup :: Fltk.Ref Fltk.GroupBase }

newtype Image
  = Image { unImage :: Fltk.Ref Fltk.Image }

newtype Widget
  = Widget { unWidgetBase :: Fltk.Ref Fltk.WidgetBase }

newtype Window
  = Window { unWindow :: Fltk.Ref Fltk.WindowBase }


class IsGroup a where
  asGroup ::
       a
    -> (forall b. Fltk.Parent b Fltk.GroupBase => Fltk.Ref b -> r)
    -> r

class IsImage a where
  asImage ::
       a
    -> (forall b. Fltk.Parent b Fltk.Image => Fltk.Ref b -> r)
    -> r

class IsWidget a where
  asWidget ::
       a
    -> (forall b. Fltk.Parent b Fltk.WidgetBase => Fltk.Ref b -> r)
    -> r
