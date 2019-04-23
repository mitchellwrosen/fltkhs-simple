module Fltk.Box
  ( -- * Box
    Box
  , new
    -- * API
    -- ** Properties
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
    -- ** Functions
  , activate
  , active
  , activeR
  , changed
  , clearActive
  , clearChanged
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
  , contains
  , copyTooltip
  , deactivate
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , flags
  , getCallback
  , getDeimage
  , getH
  , getImage
  , getLabeltype
  , getOutput
  , getParent
  , getRectangle
  , getTopWindow
  , getTopWindowOffset
  , getVisibleR
  , getW
  , getWindow
  , getX
  , getY
  , handle
  , hasCallback
  , inside
  , measureLabel
  , redraw
  , redrawLabel
  , resize
  , setActive
  , setCallback
  , setChanged
  , setColorWithBgSel
  , setDamageInside
  , setDeimage
  , setFlag
  , setImage
  , setLabeltype
  , setOutput
  , setParent
  , takeFocus
  , takesevents
  ) where

import Fltk.Types.Internal (Box(..), Group(..), Image(..), IsGroup(..),
                            IsImage(..), IsWidget(..), Window(..))

import qualified Fltk.Internal.Widget as Widget

import Data.Coerce   (coerce)
import Data.StateVar (StateVar)
import Data.Text     (Text)
import Data.Word     (Word8)
import Foreign.Ptr   (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Box             as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


new ::
     Fltk.Boxtype -- ^
  -> Fltk.Rectangle -- ^
  -> Text -- ^
  -> IO Box
new boxtype rect title =
  coerce (Fltk.boxCustomWithBoxtype boxtype rect title Nothing Nothing)

wrapped ::
     (Fltk.Ref Fltk.Box -> a)
  -> Box
  -> a
wrapped =
  coerce


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

align ::
     Box -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Widget.align

box ::
     Box -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Widget.box

color ::
     Box -- ^
  -> StateVar Fltk.Color
color =
  wrapped Widget.color

damage ::
     Box -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Widget.damage

label ::
     Box -- ^
  -> StateVar Text
label =
  wrapped Widget.label

labelColor ::
     Box -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Widget.labelColor

labelFont ::
     Box -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Widget.labelFont

labelSize ::
     Box -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Widget.labelSize

selectionColor ::
     Box -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Widget.selectionColor

tooltip ::
     Box -- ^
  -> StateVar Text
tooltip =
  wrapped Widget.tooltip

type_ ::
     Box -- ^
  -> StateVar Word8
type_ =
  wrapped Widget.type_

visible ::
     Box -- ^
  -> StateVar Bool
visible =
  wrapped Widget.visible

visibleFocus ::
     Box -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Widget.visibleFocus

when ::
     Box -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Widget.when


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

activate ::
     Box -- ^
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Box -- ^
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Box -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

changed ::
     Box -- ^
  -> IO Bool
changed =
  wrapped Fltk.changed

clearActive ::
     Box -- ^
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

clearChanged ::
     Box -- ^
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Box -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Box -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Box -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Box -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

contains ::
     IsWidget widget
  => Box -- ^
  -> widget -- ^
  -> IO Bool
contains box widget =
  asWidget widget (wrapped Fltk.contains box)

copyTooltip ::
     Box -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

deactivate ::
     Box -- ^
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Box -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Box -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

drawBackdrop ::
     Box -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Box -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Box -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawFocus ::
     Box -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle) -- ^
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Box -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments) -- ^
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

flags ::
     Box -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

getCallback ::
     Box -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getDeimage ::
     Box -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Box -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Box -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabeltype ::
     Box -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getOutput ::
     Box -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Box -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Box -- ^
  -> IO Fltk.Rectangle
getRectangle =
 wrapped Fltk.getRectangle

getTopWindow ::
     Box -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Box -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getVisibleR ::
     Box -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Box -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWindow ::
     Box -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Box -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Box -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Box -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Box -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

inside ::
     IsWidget widget
  => Box -- ^
  -> widget -- ^
  -> IO Bool
inside box widget =
  asWidget widget (wrapped Fltk.inside box)

measureLabel ::
     Box -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

redraw ::
     Box -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Box -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

resize ::
     Box -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

setActive ::
     Box -- ^
  -> IO ()
setActive =
  wrapped Fltk.setActive

setCallback ::
     Box -- ^
  -> (Box -> IO ()) -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (coerce callback)

setChanged ::
     Box -- ^
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setColorWithBgSel ::
     Box -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamageInside ::
     Box -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDeimage ::
     IsImage image
  => Box -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage box = \case
  Nothing ->
    wrapped Fltk.setDeimage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage box (Just ref))

setFlag ::
     Box -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Box -- ^
  -> Maybe image -- ^
  -> IO ()
setImage box = \case
  Nothing ->
    wrapped Fltk.setImage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage box (Just ref))

setLabeltype ::
     Box -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setOutput ::
     Box -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Box -- ^
  -> Maybe group -- ^
  -> IO ()
setParent box = \case
  Nothing ->
    wrapped Fltk.setParent box (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroup group (\ref -> wrapped Fltk.setParent box (Just ref))

takeFocus ::
     Box -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Box -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents
