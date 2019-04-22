module Fltk.WindowBase.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk

newtype WindowBase
  = WindowBase { unWindowBase :: Fltk.Ref Fltk.WindowBase }
