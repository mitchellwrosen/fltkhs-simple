module Fltk.GroupBase.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch as Fltk


newtype GroupBase
  = GroupBase { unGroupBase :: Fltk.Ref Fltk.GroupBase }

class IsGroupBase a where
  asGroupBase ::
       a
    -> (forall b. Fltk.Parent b Fltk.GroupBase => Fltk.Ref b -> r)
    -> r
