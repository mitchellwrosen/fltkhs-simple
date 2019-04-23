module Fltk.Box
  ( Box
  , new
    -- * API
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
  , clearVisible
  , clearVisibleFocus
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
  , getAlign
  , getBox
  , getCallback
  , getColor
  , getDamage
  , getDeimage
  , getH
  , getImage
  , getLabel
  , getLabelcolor
  , getLabelfont
  , getLabelsize
  , getLabeltype
  , getOutput
  , getParent
  , getRectangle
  , getSelectionColor
  , getTooltip
  , getTopWindow
  , getTopWindowOffset
  , getType_
  , getVisible
  , getVisibleFocus
  , getVisibleR
  , getW
  , getWhen
  , getWindow
  , getX
  , getY
  , handle
  , hasCallback
  , hide
  , inside
  , measureLabel
  , modifyVisibleFocus
  , redraw
  , redrawLabel
  , resize
  , setActive
  , setAlign
  , setBox
  , setCallback
  , setChanged
  , setColor
  , setColorWithBgSel
  , setDamage
  , setDamageInside
  , setDeimage
  , setFlag
  , setImage
  , setLabel
  , setLabelcolor
  , setLabelfont
  , setLabelsize
  , setLabeltype
  , setOutput
  , setParent
  , setSelectionColor
  , setTooltip
  , setType
  , setVisible
  , setVisibleFocus
  , setWhen
  , showWidget
  , takeFocus
  , takesevents
  ) where

import Fltk.Types.Internal (Box(..), Group(..), Image(..), IsGroup(..),
                            IsImage(..), IsWidget(..), Window(..))

import Data.Coerce (coerce)
import Data.Text   (Text)
import Data.Word   (Word8)
import Foreign.Ptr (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Box             as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


new ::
     Fltk.Boxtype
  -> Fltk.Rectangle
  -> Text
  -> IO Box
new boxtype rect title =
  coerce (Fltk.boxCustomWithBoxtype boxtype rect title Nothing Nothing)

wrapped ::
     (Fltk.Ref Fltk.Box -> a)
  -> Box
  -> a
wrapped =
  coerce

activate ::
     Box
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Box
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Box
  -> IO Bool
activeR =
  wrapped Fltk.activeR

changed ::
     Box
  -> IO Bool
changed =
  wrapped Fltk.changed

clearActive ::
     Box
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

clearChanged ::
     Box
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Box
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Box
  -> [Fltk.Damage]
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Box
  -> Fltk.WidgetFlag
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Box
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clearVisible ::
     Box
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Box
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

contains ::
     IsWidget widget
  => Box
  -> widget
  -> IO Bool
contains box widget =
  asWidget widget (wrapped Fltk.contains box)

copyTooltip ::
     Box
  -> Text
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

deactivate ::
     Box
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Box
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Box
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

-- draw ::
--      Box
--   -> IO ()
-- draw =
--   wrapped Fltk.draw

drawBackdrop ::
     Box
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Box
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Box
  -> Fltk.Boxtype
  -> Fltk.Color
  -> Maybe Fltk.Rectangle
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawFocus ::
     Box
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle)
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Box
  -> Maybe (Fltk.Rectangle, Fltk.Alignments)
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

flags ::
     Box
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

getAlign ::
     Box
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getBox ::
     Box
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Box
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getColor ::
     Box
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getDamage ::
     Box
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

getDeimage ::
     Box
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Box
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Box
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabel ::
     Box
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Box
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Box
  -> IO (Fltk.Font)
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Box
  -> IO (Fltk.FontSize)
getLabelsize =
  wrapped Fltk.getLabelsize

getLabeltype ::
     Box
  -> IO (Fltk.Labeltype)
getLabeltype =
  wrapped Fltk.getLabeltype

getOutput ::
     Box
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Box
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Box
  -> IO Fltk.Rectangle
getRectangle =
 wrapped Fltk.getRectangle

getSelectionColor ::
     Box
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

getTooltip ::
     Box
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

getTopWindow ::
     Box
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Box
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getType_ ::
     Box
  -> IO Word8
getType_ =
  wrapped Fltk.getType_

getVisible ::
     Box
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Box
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

getVisibleR ::
     Box
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Box
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWhen ::
     Box
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

getWindow ::
     Box
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Box
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Box
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Box
  -> Fltk.Event
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Box
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

hide ::
     Box
  -> IO ()
hide =
  wrapped Fltk.hide

inside ::
     IsWidget widget
  => Box
  -> widget
  -> IO Bool
inside box widget =
  asWidget widget (wrapped Fltk.inside box)

measureLabel ::
     Box
  -> Maybe Fltk.Width
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

modifyVisibleFocus ::
     Box
  -> Bool
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

redraw ::
     Box
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Box
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

resize ::
     Box
  -> Fltk.Rectangle
  -> IO ()
resize =
  wrapped Fltk.resize

setActive ::
     Box
  -> IO ()
setActive =
  wrapped Fltk.setActive

setAlign ::
     Box
  -> Fltk.Alignments
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

setBox ::
     Box
  -> Fltk.Boxtype
  -> IO ()
setBox =
  wrapped Fltk.setBox

setCallback ::
     Box
  -> (Box -> IO ())
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (coerce callback)

setChanged ::
     Box
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setColor ::
     Box
  -> Fltk.Color
  -> IO ()
setColor =
  wrapped Fltk.setColor

setColorWithBgSel ::
     Box
  -> Fltk.Color
  -> Fltk.Color
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamage ::
     Box
  -> [Fltk.Damage]
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

setDamageInside ::
     Box
  -> [Fltk.Damage]
  -> Fltk.Rectangle
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDeimage ::
     IsImage image
  => Box
  -> Maybe image
  -> IO ()
setDeimage box = \case
  Nothing ->
    wrapped Fltk.setDeimage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage box (Just ref))

setFlag ::
     Box
  -> Fltk.WidgetFlag
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Box
  -> Maybe image
  -> IO ()
setImage box = \case
  Nothing ->
    wrapped Fltk.setImage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage box (Just ref))

setLabel ::
     Box
  -> Text
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Box
  -> Fltk.Color
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Box
  -> Fltk.Font
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Box
  -> Fltk.FontSize
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

setLabeltype ::
     Box
  -> Fltk.Labeltype
  -> Fltk.ResolveImageLabelConflict
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setOutput ::
     Box
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Box
  -> Maybe group
  -> IO ()
setParent box = \case
  Nothing ->
    wrapped Fltk.setParent box (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroup group (\ref -> wrapped Fltk.setParent box (Just ref))

setSelectionColor ::
     Box
  -> Fltk.Color
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

setTooltip ::
     Box
  -> Text
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

setType ::
     Box
  -> Word8
  -> IO ()
setType =
  wrapped Fltk.setType

setVisible ::
     Box
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Box
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Box
  -> [Fltk.When]
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

showWidget ::
     Box
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

takeFocus ::
     Box
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Box
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents
