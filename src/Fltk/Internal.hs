module Fltk.Internal
  ( active
  , align
  , box
  , changed
  , clipChildren
  , color
  , cursorColor
  , damage
  , deimage
  , flags
  , icon
  , iconLabel
  , image
  , inputType
  , label
  , labelColor
  , labelFont
  , labelSize
  , labelType
  , mark
  , maximumSize
  , output
  , parent
  , position
  , readonly
  , resizable
  , selectionColor
  , shortcut
  , size
  , tabNav
  , textColor
  , textFont
  , textSize
  , tooltip
  , type_
  , value
  , visible
  , visibleFocus
  , when
  , wrap
  , xclass
  ) where

import Fltk.Internal.Types (Group(..), Image(..), Widget(..))

import Data.Coerce   (coerce)
import Data.Foldable (traverse_)
import Data.Functor  (void)
import Data.Maybe    (fromMaybe)
import Data.StateVar (StateVar, makeStateVar)
import Data.Text     (Text)
import Data.Word     (Word8)

import qualified Graphics.UI.FLTK.LowLevel.Base.Input      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
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

changed ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.Changed ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetChanged ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.ClearChanged ())
     , Fltk.Op (Fltk.Changed ()) r a (IO Bool)
     , Fltk.Op (Fltk.SetChanged ()) s a (IO ())
     , Fltk.Op (Fltk.ClearChanged ()) t a (IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
changed x =
  makeStateVar
    (Fltk.changed x)
    (\case
      False -> Fltk.clearChanged x
      True -> Fltk.setChanged x)

clipChildren ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.ClipChildren ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetClipChildren ())
     , Fltk.Op (Fltk.ClipChildren ()) r a (IO Bool)
     , Fltk.Op (Fltk.SetClipChildren ()) s a (Bool -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
clipChildren x =
  makeStateVar (Fltk.clipChildren x) (Fltk.setClipChildren x)

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

cursorColor ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetCursorColor ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetCursorColor ())
     , Fltk.Op (Fltk.GetCursorColor ()) r a (IO Fltk.Color)
     , Fltk.Op (Fltk.SetCursorColor ()) s a (Fltk.Color -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Color
cursorColor x =
  makeStateVar (Fltk.getCursorColor x) (Fltk.setCursorColor x)

damage ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetDamage ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.ClearDamageThenSet ())
     , Fltk.Op (Fltk.GetDamage ()) r a (IO [Fltk.Damage])
     , Fltk.Op (Fltk.ClearDamageThenSet ()) s a ([Fltk.Damage] -> IO ())
     )
  => Fltk.Ref a
  -> StateVar [Fltk.Damage]
damage x =
  makeStateVar (Fltk.getDamage x) (Fltk.clearDamageThenSet x)

deimage ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetDeimage ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetDeimage ())
     , Fltk.Op (Fltk.GetDeimage ()) r a (IO (Maybe (Fltk.Ref Fltk.Image)))
     , Fltk.Op (Fltk.SetDeimage ()) s a (Maybe (Fltk.Ref Fltk.Image) -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Image)
deimage x =
  makeStateVar
    (coerce @(IO (Maybe (Fltk.Ref Fltk.Image))) (Fltk.getDeimage x))
    (coerce @(Maybe (Fltk.Ref Fltk.Image) -> IO ()) (Fltk.setDeimage x))

flags ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.Flags ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.ClearFlag ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.SetFlag ())
     , Fltk.Op (Fltk.Flags ()) r a (IO [Fltk.WidgetFlag])
     , Fltk.Op (Fltk.ClearFlag ()) s a (Fltk.WidgetFlag -> IO ())
     , Fltk.Op (Fltk.SetFlag ()) t a (Fltk.WidgetFlag -> IO ())
     )
  => Fltk.Ref a
  -> StateVar [Fltk.WidgetFlag]
flags x =
  makeStateVar
    (Fltk.flags x)
    (\newFlags -> do
      oldFlags :: [Fltk.WidgetFlag] <- Fltk.flags x
      traverse_ (Fltk.clearFlag x :: Fltk.WidgetFlag -> IO ()) oldFlags
      traverse_ (Fltk.setFlag x :: Fltk.WidgetFlag -> IO ()) newFlags)

icon ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetIcon ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetIcon ())
     , Fltk.Op (Fltk.GetIcon ()) r a (IO (Maybe (Fltk.Ref Fltk.Image)))
     , Fltk.Op (Fltk.SetIcon ()) s a (Maybe (Fltk.Ref Fltk.Image) -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Image)
icon x =
  makeStateVar
    (coerce @(IO (Maybe (Fltk.Ref Fltk.Image))) (Fltk.getIcon x))
    (coerce @(Maybe (Fltk.Ref Fltk.Image) -> IO ()) (Fltk.setIcon x))

iconLabel ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetIconlabel ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetIconlabel ())
     , Fltk.Op (Fltk.GetIconlabel ()) r a (IO Text)
     , Fltk.Op (Fltk.SetIconlabel ()) s a (Text -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Text
iconLabel x =
  makeStateVar (Fltk.getIconlabel x) (Fltk.setIconlabel x)

image ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetImage ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetImage ())
     , Fltk.Op (Fltk.GetImage ()) r a (IO (Maybe (Fltk.Ref Fltk.Image)))
     , Fltk.Op (Fltk.SetImage ()) s a (Maybe (Fltk.Ref Fltk.Image) -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Image)
image x =
  makeStateVar
    (coerce @(IO (Maybe (Fltk.Ref Fltk.Image))) (Fltk.getImage x))
    (coerce @(Maybe (Fltk.Ref Fltk.Image) -> IO ()) (Fltk.setImage x))

inputType ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetInputType ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetInputType ())
     , Fltk.Op (Fltk.GetInputType ()) r a (IO Fltk.FlInputType)
     , Fltk.Op (Fltk.SetInputType ()) s a (Fltk.FlInputType -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.FlInputType
inputType x =
  makeStateVar (Fltk.getInputType x) (Fltk.setInputType x)

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

labelType ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetLabeltype ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetLabeltype ())
     , Fltk.Op (Fltk.GetLabeltype ()) r a (IO Fltk.Labeltype)
     , Fltk.Op (Fltk.SetLabeltype ()) s a (Fltk.Labeltype -> Fltk.ResolveImageLabelConflict -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Labeltype
labelType x =
  makeStateVar
    (Fltk.getLabeltype x)
    (\t -> Fltk.setLabeltype x t Fltk.ResolveImageLabelOverwrite)

mark ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetMark ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetMark ())
     , Fltk.Op (Fltk.GetMark ()) r a (IO Int)
     , Fltk.Op (Fltk.SetMark ()) s a (Int -> IO (Either Fltk.NoChange ()))
     )
  => Fltk.Ref a
  -> StateVar Int
mark x =
  makeStateVar
    (Fltk.getMark x)
    (void . (Fltk.setMark x :: Int -> IO (Either Fltk.NoChange ())))

maximumSize ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetMaximumSize ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetMaximumSize ())
     , Fltk.Op (Fltk.GetMaximumSize ()) r a (IO Int)
     , Fltk.Op (Fltk.SetMaximumSize ()) s a (Int -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Int
maximumSize x =
  makeStateVar (Fltk.getMaximumSize x) (Fltk.setMaximumSize x)

output ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetOutput ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetOutput ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.ClearOutput ())
     , Fltk.Op (Fltk.GetOutput ()) r a (IO Int)
     , Fltk.Op (Fltk.SetOutput ()) s a (IO ())
     , Fltk.Op (Fltk.ClearOutput ()) t a (IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
output x =
  makeStateVar
    ((/= (0::Int)) <$> Fltk.getOutput x)
    (\case
      False -> Fltk.clearOutput x
      True -> Fltk.setOutput x)

parent ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetParent ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetParent ())
     , Fltk.Op (Fltk.GetParent ()) r a (IO (Maybe (Fltk.Ref Fltk.GroupBase)))
     , Fltk.Op (Fltk.SetParent ()) s a (Maybe (Fltk.Ref Fltk.GroupBase) -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Group)
parent x =
  makeStateVar
    (coerce @(IO (Maybe (Fltk.Ref Fltk.GroupBase))) (Fltk.getParent x))
    (coerce @(Maybe (Fltk.Ref Fltk.GroupBase) -> IO ()) (Fltk.setParent x))

position ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetPosition ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetPosition ())
     , Fltk.Op (Fltk.GetPosition ()) r a (IO Int)
     , Fltk.Op (Fltk.SetPosition ()) s a (Int -> Maybe Int -> IO (Either Fltk.NoChange ()))
     )
  => Fltk.Ref a
  -> StateVar Int
position x =
  makeStateVar
    (Fltk.getPosition x)
    (\n -> void (Fltk.setPosition x n (Nothing :: Maybe Int) :: IO (Either Fltk.NoChange ())))

readonly ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetReadonly ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetReadonly ())
     , Fltk.Op (Fltk.GetReadonly ()) r a (IO Bool)
     , Fltk.Op (Fltk.SetReadonly ()) s a (Bool -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
readonly x =
  makeStateVar (Fltk.getReadonly x) (Fltk.setReadonly x)

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

shortcut ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetShortcut ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetShortcut ())
     , Fltk.Op (Fltk.GetShortcut ()) r a (IO (Maybe Fltk.ShortcutKeySequence))
     , Fltk.Op (Fltk.SetShortcut ()) s a (Fltk.ShortcutKeySequence -> IO ())
     )
  => Fltk.Ref a
  -> StateVar (Maybe Fltk.ShortcutKeySequence)
shortcut x =
  makeStateVar
    (Fltk.getShortcut x)
    (Fltk.setShortcut x .
      fromMaybe (Fltk.ShortcutKeySequence [] (Fltk.NormalKeyType '\0')))

size ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetX ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.GetY ())
     , Fltk.Match t ~ Fltk.FindOp a a (Fltk.GetW ())
     , Fltk.Match u ~ Fltk.FindOp a a (Fltk.GetH ())
     , Fltk.Match v ~ Fltk.FindOp a a (Fltk.Resize ())
     , Fltk.Op (Fltk.GetX ()) r a (IO Fltk.X)
     , Fltk.Op (Fltk.GetY ()) s a (IO Fltk.Y)
     , Fltk.Op (Fltk.GetW ()) t a (IO Fltk.Width)
     , Fltk.Op (Fltk.GetH ()) u a (IO Fltk.Height)
     , Fltk.Op (Fltk.Resize ()) v a (Fltk.Rectangle -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Rectangle
size x =
  makeStateVar
    (Fltk.Rectangle
      <$> (Fltk.Position <$> Fltk.getX x <*> Fltk.getY x)
      <*> (Fltk.Size <$> Fltk.getW x <*> Fltk.getH x))
    (Fltk.resize x)

tabNav ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetTabNav ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetTabNav ())
     , Fltk.Op (Fltk.GetTabNav ()) r a (IO Bool)
     , Fltk.Op (Fltk.SetTabNav ()) s a (Bool -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
tabNav x =
  makeStateVar (Fltk.getTabNav x) (Fltk.setTabNav x)

textColor ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetTextcolor ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetTextcolor ())
     , Fltk.Op (Fltk.GetTextcolor ()) r a (IO Fltk.Color)
     , Fltk.Op (Fltk.SetTextcolor ()) s a (Fltk.Color -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Color
textColor x =
  makeStateVar (Fltk.getTextcolor x) (Fltk.setTextcolor x)

textFont ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetTextfont ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetTextfont ())
     , Fltk.Op (Fltk.GetTextfont ()) r a (IO Fltk.Font)
     , Fltk.Op (Fltk.SetTextfont ()) s a (Fltk.Font -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.Font
textFont x =
  makeStateVar (Fltk.getTextfont x) (Fltk.setTextfont x)

textSize ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetTextsize ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetTextsize ())
     , Fltk.Op (Fltk.GetTextsize ()) r a (IO Fltk.FontSize)
     , Fltk.Op (Fltk.SetTextsize ()) s a (Fltk.FontSize -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Fltk.FontSize
textSize x =
  makeStateVar (Fltk.getTextsize x) (Fltk.setTextsize x)

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

value ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetValue ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetValue ())
     , Fltk.Op (Fltk.GetValue ()) r a (IO Text)
     , Fltk.Op (Fltk.SetValue ()) s a (Text -> IO (Either Fltk.NoChange ()))
     )
  => Fltk.Ref a
  -> StateVar Text
value x =
  makeStateVar
    (Fltk.getValue x)
    (void . (Fltk.setValue x :: Text -> IO (Either Fltk.NoChange ())))

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

wrap ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetWrap ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetWrap ())
     , Fltk.Op (Fltk.GetWrap ()) r a (IO Bool)
     , Fltk.Op (Fltk.SetWrap ()) s a (Bool -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Bool
wrap x =
  makeStateVar (Fltk.getWrap x) (Fltk.setWrap x)

xclass ::
     ( Fltk.Match r ~ Fltk.FindOp a a (Fltk.GetXclass ())
     , Fltk.Match s ~ Fltk.FindOp a a (Fltk.SetXclass ())
     , Fltk.Op (Fltk.GetXclass ()) r a (IO Text)
     , Fltk.Op (Fltk.SetXclass ()) s a (Text -> IO ())
     )
  => Fltk.Ref a
  -> StateVar Text
xclass x =
  makeStateVar (Fltk.getXclass x) (Fltk.setXclass x)
