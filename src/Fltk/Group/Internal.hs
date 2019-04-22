module Fltk.Group.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk

newtype Group
  = Group { unGroup :: Fltk.Ref Fltk.Group }
