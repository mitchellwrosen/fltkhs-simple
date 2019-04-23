module Fltk.Group
  ( -- * Group
    Group
  , IsGroup
  , new
    -- * API
  , activate
  , active
  , activeR
  , add
  , addResizable
  , begin
  , changed
  , children
  , clear
  , clearActive
  , clearChanged
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
  , clearVisible
  , clearVisibleFocus
  , clipChildren
  , contains
  , copyTooltip
  , ddfdesignKludge
  , deactivate
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawChild
  , drawChildren
  , drawFocus
  , drawLabel
  , drawOutsideLabel
  , end
  , find
  , flags
  , focus
  , getAlign
  , getArray
  , getBox
  , getCallback
  , getChild
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
  , getResizable
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
  , initSizes
  , insert
  , inside
  , measureLabel
  , modifyVisibleFocus
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , resize
  , setActive
  , setBox
  , setCallback
  , setClipChildren
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
  , setNotResizable
  , setOutput
  , setParent
  , setResizable
  , setSelectionColor
  , setTooltip
  , setType
  , setVisible
  , setVisibleFocus
  , setWhen
  , showWidget
  , takeFocus
  , takesevents
  , updateChild
  , within
  ) where

import Fltk.Types.Internal (Group(..), Image(..), IsGroup(..), IsImage(..),
                            IsWidget(..), Widget(..), Window(..))

import Data.Coerce (coerce)
import Data.Text   (Text)
import Data.Word   (Word8)
import Foreign.Ptr (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Group      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


wrapped ::
     (Fltk.Ref Fltk.GroupBase -> a)
  -> Group
  -> a
wrapped =
  coerce


new ::
     Fltk.Rectangle -- ^
  -> Text -- ^
  -> IO Group
new rect label =
  coerce (Fltk.groupNew rect (Just label))


activate ::
     Group -- ^
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Group -- ^
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Group -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

add ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
add group widget =
  asWidget widget (wrapped Fltk.add group)

addResizable ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
addResizable group widget =
  asWidget widget (wrapped Fltk.addResizable group)

begin ::
     Group -- ^
  -> IO ()
begin =
  wrapped Fltk.begin

changed ::
     Group -- ^
  -> IO Bool
changed =
  wrapped Fltk.changed

children ::
     Group -- ^
  -> IO Int
children =
  wrapped Fltk.children

clear ::
     Group -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

clearActive ::
     Group -- ^
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

clearChanged ::
     Group -- ^
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Group -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Group -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Group -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Group -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clearVisible ::
     Group -- ^
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Group -- ^
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

clipChildren ::
     Group -- ^
  -> IO Bool
clipChildren =
  wrapped Fltk.clipChildren

contains ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Bool
contains group widget =
  asWidget widget (wrapped Fltk.contains group)

copyTooltip ::
     Group -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

ddfdesignKludge ::
     Group -- ^
  -> IO (Maybe Widget)
ddfdesignKludge =
  coerce (wrapped Fltk.ddfdesignKludge)

deactivate ::
     Group -- ^
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Group -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Group -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

drawBackdrop ::
     Group -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Group -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Group -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawChild ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
drawChild group widget =
  asWidget widget (wrapped Fltk.drawChild group)

drawChildren ::
     Group -- ^
  -> IO ()
drawChildren =
  wrapped Fltk.drawChildren

drawFocus ::
     Group -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle) -- ^
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Group -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments) -- ^
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

drawOutsideLabel ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
drawOutsideLabel group widget =
  asWidget widget (wrapped Fltk.drawOutsideLabel group)

end ::
     Group -- ^
  -> IO ()
end =
  wrapped Fltk.end

find ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Fltk.AtIndex
find group widget =
  asWidget widget (wrapped Fltk.find group)

flags ::
     Group -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

focus ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
focus group widget =
  asWidget widget (wrapped Fltk.focus group)

getAlign ::
     Group -- ^
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getArray ::
     Group -- ^
  -> IO [Widget]
getArray =
  coerce (wrapped Fltk.getArray)

getBox ::
     Group -- ^
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Group -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getChild ::
     Group -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
getChild =
  coerce (wrapped Fltk.getChild)

getColor ::
     Group -- ^
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getDamage ::
     Group -- ^
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

getDeimage ::
     Group -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Group -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Group -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabel ::
     Group -- ^
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Group -- ^
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Group -- ^
  -> IO Fltk.Font
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Group -- ^
  -> IO Fltk.FontSize
getLabelsize =
  wrapped Fltk.getLabelsize

getLabeltype ::
     Group -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getOutput ::
     Group -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Group -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Group -- ^
  -> IO Fltk.Rectangle
getRectangle =
  wrapped Fltk.getRectangle

getResizable ::
     Group -- ^
  -> IO (Maybe Widget)
getResizable =
  coerce (wrapped Fltk.getResizable)

getSelectionColor ::
     Group -- ^
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

getTooltip ::
     Group -- ^
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

getTopWindow ::
     Group -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Group -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getType_ ::
     Group -- ^
  -> IO Word8
getType_ =
  wrapped Fltk.getType_

getVisible ::
     Group -- ^
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Group -- ^
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

getVisibleR ::
     Group -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Group -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWhen ::
     Group -- ^
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

getWindow ::
     Group -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Group -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Group -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Group -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Group -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

hide ::
     Group -- ^
  -> IO ()
hide =
  wrapped Fltk.hide

initSizes ::
     Group -- ^
  -> IO ()
initSizes =
  wrapped Fltk.initSizes

insert ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
insert group widget index =
  asWidget widget (\ref -> wrapped Fltk.insert group ref index)

-- insertBefore :: (Parent a WidgetBase) => Group -> Ref a -> Ref b -> IO ()
-- insertBefore=
--   wrapped Fltk.insertBefore

inside ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Bool
inside group widget =
  asWidget widget (wrapped Fltk.inside group)

measureLabel ::
     Group -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

modifyVisibleFocus ::
     Group -- ^
  -> Bool -- ^
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

redraw ::
     Group -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Group -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

removeIndex ::
     Group -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
removeIndex =
  wrapped Fltk.removeIndex

removeWidget ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
removeWidget group widget =
  asWidget widget (wrapped Fltk.removeWidget group)

resize ::
     Group -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

setActive ::
     Group -- ^
  -> IO ()
setActive =
  wrapped Fltk.setActive

setBox ::
     Group -- ^
  -> Fltk.Boxtype -- ^
  -> IO ()
setBox =
  wrapped Fltk.setBox

setCallback ::
     Group -- ^
  -> (Group -> IO ()) -- ^
  -> IO ()
setCallback group callback =
  wrapped Fltk.setCallback group (coerce callback)

setClipChildren ::
     Group -- ^
  -> Bool -- ^
  -> IO ()
setClipChildren =
  wrapped Fltk.setClipChildren

setColor ::
     Group -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColor =
  wrapped Fltk.setColor

setColorWithBgSel ::
     Group -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamage ::
     Group -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

setDamageInside ::
     Group -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDeimage ::
     IsImage image
  => Group -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage group = \case
  Nothing ->
    wrapped Fltk.setDeimage group (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage group (Just ref))

setFlag ::
     Group -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Group -- ^
  -> Maybe image -- ^
  -> IO ()
setImage group = \case
  Nothing ->
    wrapped Fltk.setImage group (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage group (Just ref))

setLabel ::
     Group -- ^
  -> Text -- ^
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Group -- ^
  -> Fltk.Color -- ^
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Group -- ^
  -> Fltk.Font -- ^
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Group -- ^
  -> Fltk.FontSize -- ^
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

setLabeltype ::
     Group -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setNotResizable ::
     Group -- ^
  -> IO ()
setNotResizable =
  wrapped Fltk.setNotResizable

setOutput ::
     Group -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Group -- ^
  -> Maybe group -- ^
  -> IO ()
setParent group = \case
  Nothing ->
    wrapped Fltk.setParent group (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just parent ->
    asGroup parent (\ref -> wrapped Fltk.setParent group (Just ref))

setResizable ::
     IsWidget widget
  => Group -- ^
  -> Maybe widget -- ^
  -> IO ()
setResizable group = \case
  Nothing ->
    wrapped Fltk.setResizable group (Nothing @(Fltk.Ref Fltk.WidgetBase))
  Just widget ->
    asWidget widget (\ref -> wrapped Fltk.setResizable group (Just ref))

setSelectionColor ::
     Group -- ^
  -> Fltk.Color -- ^
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

setTooltip ::
     Group -- ^
  -> Text -- ^
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

setType ::
     Group -- ^
  -> Word8 -- ^
  -> IO ()
setType =
  wrapped Fltk.setType

setVisible ::
     Group -- ^
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Group -- ^
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Group -- ^
  -> [Fltk.When] -- ^
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

showWidget ::
     Group -- ^
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

takeFocus ::
     Group -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Group -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents

updateChild ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
updateChild group widget =
  asWidget widget (wrapped Fltk.updateChild group)

within ::
     Group -- ^
  -> IO a -- ^
  -> IO a
within =
  wrapped Fltk.within
