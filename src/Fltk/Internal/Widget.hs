module Fltk.Internal.Widget
  ( active
  , align
  , box
  , color
  , damage
  , label
  , labelColor
  , labelFont
  , labelSize
  , selectionColor
  , tooltip
  , type_
  , visible
  , visibleFocus
  , when
  ) where

import Data.StateVar (StateVar, makeStateVar)
import Data.Text     (Text)
import Data.Word     (Word8)

import qualified Graphics.UI.FLTK.LowLevel.Dispatch        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


active ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.Active ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.Activate ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.Deactivate ())
     , Fltk.Op (Fltk.Active ()) r a (IO Bool)
     , Fltk.Op (Fltk.Activate ()) s a (IO ())
     , Fltk.Op (Fltk.Deactivate ()) t a (IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
active x =
  makeStateVar
    (Fltk.active x)
    (\case
      False -> Fltk.deactivate x
      True -> Fltk.activate x)

align ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetAlign ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetAlign ())
     , Fltk.Op (Fltk.GetAlign ()) r a (IO Fltk.Alignments)
     , Fltk.Op (Fltk.SetAlign ()) s a (Fltk.Alignments -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Alignments
align x =
  makeStateVar (Fltk.getAlign x) (Fltk.setAlign x)

box ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetBox ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetBox ())
     , Fltk.Op (Fltk.GetBox ()) r a (IO Fltk.Boxtype)
     , Fltk.Op (Fltk.SetBox ()) s a (Fltk.Boxtype -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Boxtype
box x =
  makeStateVar (Fltk.getBox x) (Fltk.setBox x)

color ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetColor ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetColor ())
     , Fltk.Op (Fltk.GetColor ()) r a (IO Fltk.Color)
     , Fltk.Op (Fltk.SetColor ()) s a (Fltk.Color -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Color
color x =
  makeStateVar (Fltk.getColor x) (Fltk.setColor x)

damage ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetDamage ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetDamage ())
     , Fltk.Op (Fltk.GetDamage ()) r a (IO [Fltk.Damage])
     , Fltk.Op (Fltk.SetDamage ()) s a ([Fltk.Damage] -> IO ())
     )
  => Fltk.Ref a
  -> StateVar [Fltk.Damage]
damage x =
  makeStateVar (Fltk.getDamage x) (Fltk.setDamage x)

label ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetLabel ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetLabel ())
     , Fltk.Op (Fltk.GetLabel ()) r a (IO Text)
     , Fltk.Op (Fltk.SetLabel ()) s a (Text -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Text
label x =
  makeStateVar (Fltk.getLabel x) (Fltk.setLabel x)

labelColor ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetLabelcolor ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetLabelcolor ())
     , Fltk.Op (Fltk.GetLabelcolor ()) r a (IO Fltk.Color)
     , Fltk.Op (Fltk.SetLabelcolor ()) s a (Fltk.Color -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Color
labelColor x =
  makeStateVar (Fltk.getLabelcolor x) (Fltk.setLabelcolor x)

labelFont ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetLabelfont ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetLabelfont ())
     , Fltk.Op (Fltk.GetLabelfont ()) r a (IO Fltk.Font)
     , Fltk.Op (Fltk.SetLabelfont ()) s a (Fltk.Font -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Font
labelFont x =
  makeStateVar (Fltk.getLabelfont x) (Fltk.setLabelfont x)

labelSize ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetLabelsize ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetLabelsize ())
     , Fltk.Op (Fltk.GetLabelsize ()) r a (IO Fltk.FontSize)
     , Fltk.Op (Fltk.SetLabelsize ()) s a (Fltk.FontSize -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.FontSize
labelSize x =
  makeStateVar (Fltk.getLabelsize x) (Fltk.setLabelsize x)

selectionColor ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetSelectionColor ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetSelectionColor ())
     , Fltk.Op (Fltk.GetSelectionColor ()) r a (IO Fltk.Color)
     , Fltk.Op (Fltk.SetSelectionColor ()) s a (Fltk.Color -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Color
selectionColor x =
  makeStateVar (Fltk.getSelectionColor x) (Fltk.setSelectionColor x)

tooltip ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetTooltip ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetTooltip ())
     , Fltk.Op (Fltk.GetTooltip ()) r a (IO Text)
     , Fltk.Op (Fltk.SetTooltip ()) s a (Text -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Text
tooltip x =
  makeStateVar (Fltk.getTooltip x) (Fltk.setTooltip x)

type_ ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetType_ ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetType ())
     , Fltk.Op (Fltk.GetType_ ()) r a (IO Word8)
     , Fltk.Op (Fltk.SetType ()) s a (Word8 -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Word8
type_ x =
  makeStateVar (Fltk.getType_ x) (Fltk.setType x)

visible ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetVisible ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.Hide ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.ShowWidget ())
     , Fltk.Op (Fltk.GetVisible ()) r a (IO Bool)
     , Fltk.Op (Fltk.Hide ()) s a (IO ())
     , Fltk.Op (Fltk.ShowWidget ()) t a (IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
visible x =
  makeStateVar
    (Fltk.getVisible x)
    (\case
      False -> Fltk.hide x
      True -> Fltk.showWidget x)

visibleFocus ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetVisibleFocus ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.ModifyVisibleFocus ())
     , Fltk.Op (Fltk.GetVisibleFocus ()) r a (IO Bool)
     , Fltk.Op (Fltk.ModifyVisibleFocus ()) s a (Bool -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
visibleFocus x =
  makeStateVar (Fltk.getVisibleFocus x) (Fltk.modifyVisibleFocus x)

when ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetWhen ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetWhen ())
     , Fltk.Op (Fltk.GetWhen ()) r a (IO [Fltk.When])
     , Fltk.Op (Fltk.SetWhen ()) s a ([Fltk.When] -> IO ())
     )
  => Fltk.Ref a
  -> StateVar [Fltk.When]
when x =
  makeStateVar (Fltk.getWhen x) (Fltk.setWhen x)
