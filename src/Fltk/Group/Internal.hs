module Fltk.Group.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch as Fltk

newtype Group
  = Group { unGroup :: Fltk.Ref Fltk.GroupBase }

class IsGroup a where
  asGroup ::
       a
    -> (forall b. Fltk.Parent b Fltk.GroupBase => Fltk.Ref b -> r)
    -> r
