module Fltk.Image.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk

newtype Image
  = Image { unImage :: Fltk.Ref Fltk.Image }

class IsImage a where
  asImage ::
       a
    -> (forall b. Fltk.Parent b Fltk.Image => Fltk.Ref b -> r)
    -> r
