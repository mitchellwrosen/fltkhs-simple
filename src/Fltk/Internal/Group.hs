module Fltk.Internal.Group
  ( resizable
  ) where

import Fltk.Internal.Types (Widget(..))

import Data.Coerce   (coerce)
import Data.StateVar (StateVar, makeStateVar)

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk


resizable ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetResizable ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetResizable ())
     , Fltk.Op (Fltk.GetResizable ()) r a (IO (Maybe (Fltk.Ref Fltk.WidgetBase)))
     , Fltk.Op (Fltk.SetResizable ()) s a (Maybe (Fltk.Ref Fltk.WidgetBase) -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Widget)
resizable x =
  makeStateVar
    (coerce @(IO (Maybe (Fltk.Ref Fltk.WidgetBase))) (Fltk.getResizable x))
    (coerce @(Maybe (Fltk.Ref Fltk.WidgetBase) -> IO ()) (Fltk.setResizable x))
