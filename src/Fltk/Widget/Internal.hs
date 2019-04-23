module Fltk.Widget.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk


newtype Widget
  = Widget { unWidgetBase :: Fltk.Ref Fltk.Widget }

class IsWidget a where
  asWidget ::
       a
    -> (forall b. Fltk.Parent b Fltk.WidgetBase => Fltk.Ref b -> r)
    -> r
