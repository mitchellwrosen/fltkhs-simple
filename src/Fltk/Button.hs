module Fltk.Button
  ( Button
  , new
    -- * API
  , activate
  , active
  , activeR
  , changed
  , clear
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
  , getDownBox
  , getDownColor
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
  , getShortcut
  , getTooltip
  , getTopWindow
  , getTopWindowOffset
  , getType_
  , getValue
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
  , set
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
  , setDownBox
  , setDownColor
  , setFlag
  , setImage
  , setLabel
  , setLabelcolor
  , setLabelfont
  , setLabelsize
  , setLabeltype
  , setonly
  , setOutput
  , setParent
  , setSelectionColor
  , setShortcut
  , setTooltip
  , setType
  , setValue
  , setVisible
  , setVisibleFocus
  , setWhen
  , showWidget
  , takeFocus
  , takesevents
  ) where

import Fltk.Group.Internal  (Group(..), IsGroup(..))
import Fltk.Image.Internal  (Image(..), IsImage(..))
import Fltk.Widget.Internal (IsWidget(..))
import Fltk.Window.Internal (Window(..))

import Data.Coerce (coerce)
import Data.Text   (Text)
import Foreign.Ptr (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Button     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Button          as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


newtype Button
  = Button { unButton :: Fltk.Ref Fltk.Button }

instance IsWidget Button where
  asWidget ::
       Button
    -> (forall x. Fltk.Parent x Fltk.WidgetBase => Fltk.Ref x -> r)
    -> r
  asWidget button f =
    f (unButton button)

wrapped ::
     (Fltk.Ref Fltk.Button -> a)
  -> Button
  -> a
wrapped =
  coerce

new ::
     Fltk.Rectangle -- ^
  -> Text -- ^
  -> IO Button
new bounds label =
  Button <$> Fltk.buttonNew bounds (Just label)

activate ::
     Button -- ^
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Button -- ^
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Button -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

changed ::
     Button -- ^
  -> IO Bool
changed =
  wrapped Fltk.changed

clear ::
     Button -- ^
  -> IO Bool
clear =
  wrapped Fltk.clear

clearActive ::
     Button -- ^
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

clearChanged ::
     Button -- ^
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Button -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Button -- ^
  -> [Fltk.Damage]
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Button -- ^
  -> Fltk.WidgetFlag
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Button -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clearVisible ::
     Button -- ^
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Button -- ^
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

contains ::
     IsWidget widget
  => Button -- ^
  -> widget -- ^
  -> IO Bool
contains button widget =
  asWidget widget (Fltk.contains (unButton button))

copyTooltip ::
     Button -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

deactivate ::
     Button -- ^
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Button -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Button -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

-- draw ::
--      Button
--   -> IO ()
-- draw button =
--   Fltk.draw (unButton button)

drawBackdrop ::
     Button -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Button -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Button -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawFocus ::
     Button -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle)
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Button -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments)
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

flags ::
     Button -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

getAlign ::
     Button -- ^
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getBox ::
     Button -- ^
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Button -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getColor ::
     Button -- ^
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getDamage ::
     Button -- ^
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

getDeimage ::
     Button -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getDownBox ::
     Button -- ^
  -> IO Fltk.Boxtype
getDownBox =
  wrapped Fltk.getDownBox

getDownColor ::
     Button -- ^
  -> IO Fltk.Color
getDownColor =
  wrapped Fltk.getDownColor

getH ::
     Button -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Button -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabel ::
     Button -- ^
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Button -- ^
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Button -- ^
  -> IO Fltk.Font
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Button -- ^
  -> IO Fltk.FontSize
getLabelsize =
  wrapped Fltk.getLabelsize

getLabeltype ::
     Button -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getOutput ::
     Button -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Button -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Button -- ^
  -> IO Fltk.Rectangle
getRectangle =
  wrapped Fltk.getRectangle

getSelectionColor ::
     Button -- ^
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

getShortcut ::
     Button -- ^
  -> IO (Maybe Fltk.ShortcutKeySequence)
getShortcut =
  wrapped Fltk.getShortcut

getTooltip ::
     Button -- ^
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

getTopWindow ::
     Button -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Button -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getType_ ::
     Button -- ^
  -> IO Fltk.ButtonType
getType_ =
  wrapped Fltk.getType_

getValue ::
     Button -- ^
  -> IO Bool
getValue =
  wrapped Fltk.getValue

getVisible ::
     Button -- ^
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Button -- ^
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

getVisibleR ::
     Button -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Button -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWhen ::
     Button -- ^
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

getWindow ::
     Button -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Button -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Button -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Button -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Button -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

hide ::
     Button -- ^
  -> IO ()
hide =
  wrapped Fltk.hide

inside ::
     IsWidget widget
  => Button -- ^
  -> widget -- ^
  -> IO Bool
inside button widget =
  asWidget widget (Fltk.inside (unButton button))

measureLabel ::
     Button -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

modifyVisibleFocus ::
     Button -- ^
  -> Bool -- ^
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

redraw ::
     Button -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Button -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

resize ::
     Button -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

set ::
     Button -- ^
  -> IO Bool
set =
  wrapped Fltk.set

setActive ::
     Button -- ^
  -> IO ()
setActive =
  wrapped Fltk.setActive

setAlign ::
     Button -- ^
  -> Fltk.Alignments
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

setBox ::
     Button -- ^
  -> Fltk.Boxtype -- ^
  -> IO ()
setBox =
  wrapped Fltk.setBox

setCallback ::
     Button -- ^
  -> (Button -> IO ()) -- ^
  -> IO ()
setCallback button callback =
  Fltk.setCallback (unButton button) (coerce callback)

setChanged ::
     Button  -- ^
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setColor ::
     Button -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColor =
  wrapped Fltk.setColor

setColorWithBgSel ::
     Button -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamage ::
     Button -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

setDamageInside ::
     Button -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDeimage ::
     IsImage image
  => Button -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage button = \case
  Nothing ->
    Fltk.setDeimage (unButton button) (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> Fltk.setDeimage (unButton button) (Just ref))

setDownBox ::
     Button -- ^
  -> Fltk.Boxtype -- ^
  -> IO ()
setDownBox =
  wrapped Fltk.setDownBox

setDownColor ::
     Button -- ^
  -> Fltk.Color -- ^
  -> IO ()
setDownColor =
  wrapped Fltk.setDownColor

setFlag ::
     Button -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Button -- ^
  -> Maybe image -- ^
  -> IO ()
setImage button = \case
  Nothing ->
    Fltk.setImage (unButton button) (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> Fltk.setImage (unButton button) (Just ref))

setLabel ::
     Button -- ^
  -> Text -- ^
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Button -- ^
  -> Fltk.Color -- ^
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Button -- ^
  -> Fltk.Font -- ^
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Button -- ^
  -> Fltk.FontSize -- ^
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

setLabeltype ::
     Button -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setonly ::
     Button -- ^
  -> IO ()
setonly =
  wrapped Fltk.setonly

setOutput ::
     Button -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Button -- ^
  -> Maybe group -- ^
  -> IO ()
setParent button = \case
  Nothing ->
    Fltk.setParent (unButton button) (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroup group (\ref -> Fltk.setParent (unButton button) (Just ref))

setSelectionColor ::
     Button -- ^
  -> Fltk.Color -- ^
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

setShortcut ::
     Button -- ^
  -> Fltk.ShortcutKeySequence -- ^
  -> IO ()
setShortcut =
  wrapped Fltk.setShortcut

setTooltip ::
     Button -- ^
  -> Text -- ^
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

setType ::
     Button -- ^
  -> Fltk.ButtonType -- ^
  -> IO ()
setType =
  wrapped Fltk.setType

setValue ::
     Button -- ^
  -> Bool -- ^
  -> IO Bool
setValue =
  wrapped Fltk.setValue

setVisible ::
     Button -- ^
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Button -- ^
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Button -- ^
  -> [Fltk.When] -- ^
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

showWidget ::
     Button -- ^
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

takeFocus ::
     Button -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Button -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents
