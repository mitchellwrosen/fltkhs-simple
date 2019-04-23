module Fltk.WidgetBase.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk

newtype WidgetBase
  = WidgetBase { unWidgetBase :: Fltk.Ref Fltk.WidgetBase }

class IsWidgetBase a where
  asWidgetBase ::
       a
    -> (forall b. Fltk.Parent b Fltk.WidgetBase => Fltk.Ref b -> r)
    -> r
