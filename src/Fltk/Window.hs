module Fltk.Window
  ( Window
  , IsWindow
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
  , clearBorder
  , clearChanged
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
  , clearVisible
  , clearVisibleFocus
  , clipChildren
  , contains
  , copyLabel
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
  , flush
  , focus
  , freePosition
  , fullscreenOff
  , getAlign
  , getArray
  , getBorder
  , getBox
  , getCallback
  , getChild
  , getColor
  , getDamage
  , getDecoratedH
  , getDecoratedW
  , getDeimage
  , getH
  , getIcon
  , getIconlabel
  , getImage
  , getLabel
  , getLabelcolor
  , getLabelfont
  , getLabelsize
  , getLabeltype
  , getMenuWindow
  , getModal
  , getOutput
  , getOverride
  , getParent
  , getRectangle
  , getResizable
  , getSelectionColor
  , getTooltip
  , getTooltipWindow
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
  , getXRoot
  , getXclass
  , getY
  , getYRoot
  , handle
  , hasCallback
  , hide
  , hotSpot
  , iconize
  , initSizes
  , insert
  , inside
  , makeCurrent
  , makeFullscreen
  , measureLabel
  , modifyVisibleFocus
  , nonModal
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , resize
  , setActive
  , setAlign
  , setBorder
  , setBox
  , setCallback
  , setChanged
  , setClipChildren
  , setColor
  , setColorWithBgSel
  , setCursor
  , setCursorWithFgBg
  , setDamage
  , setDamageInside
  , setDefaultCursor
  , setDefaultCursorWithFgBg
  , setDeimage
  , setFlag
  , setIcon
  , setIconlabel
  , setImage
  , setLabel
  , setLabelcolor
  , setLabelfont
  , setLabelsize
  , setLabeltype
  , setLabelWithIconlabel
  , setMenuWindow
  , setModal
  , setNonModal
  , setNotResizable
  , setOutput
  , setOverride
  , setParent
  , setResizable
  , setSelectionColor
  , setTooltip
  , setTooltipWindow
  , setType
  , setVisible
  , setVisibleFocus
  , setWhen
  , setXclass
  , shown
  , showWidget
  , sizeRange
  , sizeRangeWithArgs
  , takeFocus
  , takesevents
  , updateChild
  , waitForExpose
  , within
  ) where

import Fltk.Types.Internal (Group(..), Image(..), IsGroup(..), IsImage(..),
                            IsWidget(..), IsWindow(..), Widget(..), Window(..))

import Data.Coerce                          (coerce)
import Data.Text                            (Text)
import Foreign.Ptr                          (FunPtr)
import Graphics.UI.FLTK.LowLevel.Base.Group ()
import Graphics.UI.FLTK.LowLevel.Window     ()

import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Window     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


wrapped ::
     (Fltk.Ref Fltk.WindowBase -> a)
  -> Window
  -> a
wrapped =
  coerce


new ::
     Fltk.Size -- ^
  -> Maybe Fltk.Position -- ^
  -> Text -- ^
  -> IO Window
new size pos title =
  coerce (Fltk.windowNew size pos (Just title))


activate ::
     Window -- ^
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Window -- ^
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Window -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

add ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
add window widget =
  asWidget widget (wrapped Fltk.add window)

addResizable ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
addResizable window widget =
  asWidget widget (wrapped Fltk.addResizable window)

begin ::
     Window -- ^
  -> IO ()
begin =
  wrapped Fltk.begin

changed ::
     Window -- ^
  -> IO Bool
changed =
  wrapped Fltk.changed

children ::
     Window -- ^
  -> IO Int
children =
  wrapped Fltk.children

clear ::
     Window -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

clearActive ::
     Window -- ^
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

clearBorder ::
     Window -- ^
  -> IO ()
clearBorder =
  wrapped Fltk.clearBorder

clearChanged ::
     Window -- ^
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Window -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Window -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Window -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Window -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clearVisible ::
     Window -- ^
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Window -- ^
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

clipChildren ::
     Window -- ^
  -> IO Bool
clipChildren =
  wrapped Fltk.clipChildren

contains ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO Bool
contains box widget =
  asWidget widget (wrapped Fltk.contains box)

copyLabel ::
     Window -- ^
  -> Text -- ^
  -> IO ()
copyLabel =
  wrapped Fltk.copyLabel

copyTooltip ::
     Window -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

ddfdesignKludge ::
     Window -- ^
  -> IO (Maybe Widget)
ddfdesignKludge =
  coerce (wrapped Fltk.ddfdesignKludge)

deactivate ::
     Window -- ^
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Window -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Window -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

-- draw ::
--      Window
--   -> IO ()
-- draw =
--   wrapped Fltk.draw

drawBackdrop ::
     Window -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Window -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Window -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawChild ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
drawChild window widget =
  asWidget widget (wrapped Fltk.drawChild window)

drawChildren ::
     Window -- ^
  -> IO ()
drawChildren =
  wrapped Fltk.drawChildren

drawFocus ::
     Window -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle) -- ^
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Window -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments) -- ^
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

drawOutsideLabel ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
drawOutsideLabel window widget =
  asWidget widget (wrapped Fltk.drawOutsideLabel window)

end ::
     Window -- ^
  -> IO ()
end =
  wrapped Fltk.end

find ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO Fltk.AtIndex
find window widget =
  asWidget widget (wrapped Fltk.find window)

flags ::
     Window -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

flush ::
     Window -- ^
  -> IO ()
flush =
  wrapped Fltk.flush

focus ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
focus window widget =
  asWidget widget (wrapped Fltk.focus window)

freePosition ::
     Window -- ^
  -> IO ()
freePosition =
  wrapped Fltk.freePosition

fullscreenOff ::
     Window -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
fullscreenOff =
  wrapped Fltk.fullscreenOff

getAlign ::
     Window -- ^
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getArray ::
     Window -- ^
  -> IO [Widget]
getArray =
  coerce (wrapped Fltk.getArray)

getBorder ::
     Window -- ^
  -> IO Bool
getBorder =
  wrapped Fltk.getBorder

getBox ::
     Window -- ^
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Window -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getChild ::
     Window -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
getChild =
  coerce (wrapped Fltk.getChild)

getColor ::
     Window -- ^
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getDamage ::
     Window -- ^
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

getDecoratedH ::
     Window -- ^
  -> IO Int
getDecoratedH =
  wrapped Fltk.getDecoratedH

getDecoratedW ::
     Window -- ^
  -> IO Int
getDecoratedW =
  wrapped Fltk.getDecoratedW

getDeimage ::
     Window -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Window -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getIcon ::
     Window -- ^
  -> IO (Maybe Image)
getIcon =
  coerce (wrapped Fltk.getIcon)

getIconlabel ::
     Window -- ^
  -> IO Text
getIconlabel =
  wrapped Fltk.getIconlabel

getImage ::
     Window -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabel ::
     Window -- ^
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Window -- ^
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Window -- ^
  -> IO Fltk.Font
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Window -- ^
  -> IO Fltk.FontSize
getLabelsize =
  wrapped Fltk.getLabelsize

getLabeltype ::
     Window -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getMenuWindow ::
     Window -- ^
  -> IO Bool
getMenuWindow =
  wrapped Fltk.getMenuWindow

getModal ::
     Window -- ^
  -> IO Bool
getModal =
  wrapped Fltk.getModal

getOutput ::
     Window -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getOverride ::
     Window -- ^
  -> IO Bool
getOverride =
  wrapped Fltk.getOverride

getParent ::
     Window -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Window -- ^
  -> IO Fltk.Rectangle
getRectangle =
 wrapped Fltk.getRectangle

getResizable ::
     Window -- ^
  -> IO (Maybe Widget)
getResizable =
  coerce (wrapped Fltk.getResizable)

getSelectionColor ::
     Window -- ^
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

getTooltip ::
     Window -- ^
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

getTooltipWindow ::
     Window -- ^
  -> IO Bool
getTooltipWindow =
  wrapped Fltk.getTooltipWindow

getTopWindow ::
     Window -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Window -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getType_ ::
     Window -- ^
  -> IO Fltk.WindowType
getType_ =
  wrapped Fltk.getType_

getVisible ::
     Window -- ^
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Window -- ^
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

getVisibleR ::
     Window -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Window -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWhen ::
     Window -- ^
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

getWindow ::
     Window -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Window -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getXRoot ::
     Window -- ^
  -> IO Int
getXRoot =
  wrapped Fltk.getXRoot

getXclass ::
     Window -- ^
  -> IO Text
getXclass =
  wrapped Fltk.getXclass

getY ::
     Window -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

getYRoot ::
     Window -- ^
  -> IO Int
getYRoot =
  wrapped Fltk.getYRoot

handle ::
     Window -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Window -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

hide ::
     Window -- ^
  -> IO ()
hide =
  wrapped Fltk.hide

hotSpot ::
     Window -- ^
  -> Fltk.PositionSpec -- ^
  -> Maybe Bool -- ^
  -> IO ()
hotSpot =
  wrapped Fltk.hotSpot

iconize ::
     Window -- ^
  -> IO ()
iconize =
  wrapped Fltk.iconize

initSizes ::
     Window -- ^
  -> IO ()
initSizes =
  wrapped Fltk.initSizes

insert ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
insert window widget index =
  asWidget widget (\ref -> wrapped Fltk.insert window ref index)

-- insertBefore ::
--      IsWidget widget
--   => Window Group
--   -> widget
--   -> Ref b
--   -> IO ()

inside ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO Bool
inside box widget =
  asWidget widget (wrapped Fltk.inside box)

makeCurrent ::
     Window -- ^
  -> IO ()
makeCurrent =
  wrapped Fltk.makeCurrent

makeFullscreen ::
     Window -- ^
  -> IO ()
makeFullscreen =
  wrapped Fltk.makeFullscreen

measureLabel ::
     Window -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

modifyVisibleFocus ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

nonModal ::
     Window -- ^
  -> IO Bool
nonModal =
  wrapped Fltk.nonModal

redraw ::
     Window -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Window -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

removeIndex ::
     Window -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
removeIndex =
  wrapped Fltk.removeIndex

removeWidget ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
removeWidget window widget =
  asWidget widget (wrapped Fltk.removeWidget window)

resize ::
     Window -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

setActive ::
     Window -- ^
  -> IO ()
setActive =
  wrapped Fltk.setActive

setAlign ::
     Window -- ^
  -> Fltk.Alignments -- ^
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

setBorder ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
setBorder =
  wrapped Fltk.setBorder

setBox ::
     Window -- ^
  -> Fltk.Boxtype -- ^
  -> IO ()
setBox =
  wrapped Fltk.setBox

setCallback ::
     Window -- ^
  -> (Window -> IO ()) -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (coerce callback)

setChanged ::
     Window -- ^
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setClipChildren ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
setClipChildren =
  wrapped Fltk.setClipChildren

setColor ::
     Window -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColor =
  wrapped Fltk.setColor

setColorWithBgSel ::
     Window -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setCursor ::
     Window -- ^
  -> Fltk.Cursor -- ^
  -> IO ()
setCursor =
  wrapped Fltk.setCursor

setCursorWithFgBg ::
     Window -- ^
  -> Fltk.Cursor -- ^
  -> (Maybe Fltk.Color, Maybe Fltk.Color) -- ^
  -> IO ()
setCursorWithFgBg =
  wrapped Fltk.setCursorWithFgBg

setDamage ::
     Window -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

setDamageInside ::
     Window -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDefaultCursor ::
     Window -- ^
  -> Fltk.CursorType -- ^
  -> IO ()
setDefaultCursor =
  wrapped Fltk.setDefaultCursor

setDefaultCursorWithFgBg ::
     Window -- ^
  -> Fltk.CursorType -- ^
  -> (Maybe Fltk.Color, Maybe Fltk.Color) -- ^
  -> IO ()
setDefaultCursorWithFgBg =
  wrapped Fltk.setDefaultCursorWithFgBg

setDeimage ::
     IsImage image
  => Window -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage box = \case
  Nothing ->
    wrapped Fltk.setDeimage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage box (Just ref))

setFlag ::
     Window -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setIcon ::
     IsImage image
  => Window-- ^
  -> Maybe image-- ^
  -> IO ()
setIcon window = \case
  Nothing ->
    wrapped Fltk.setIcon window (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setIcon window (Just ref))

setIconlabel ::
     Window -- ^
  -> Text -- ^
  -> IO ()
setIconlabel =
  wrapped Fltk.setIconlabel

setImage ::
     IsImage image
  => Window -- ^
  -> Maybe image -- ^
  -> IO ()
setImage box = \case
  Nothing ->
    wrapped Fltk.setImage box (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage box (Just ref))

setLabel ::
     Window -- ^
  -> Text -- ^
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Window -- ^
  -> Fltk.Color -- ^
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Window -- ^
  -> Fltk.Font -- ^
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Window -- ^
  -> Fltk.FontSize -- ^
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

setLabeltype ::
     Window -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setLabelWithIconlabel ::
     Window -- ^
  -> Text -- ^
  -> Text -- ^
  -> IO ()
setLabelWithIconlabel =
  wrapped Fltk.setLabelWithIconlabel

setMenuWindow ::
     Window -- ^
  -> IO ()
setMenuWindow =
  wrapped Fltk.setMenuWindow

setModal ::
     Window -- ^
  -> IO ()
setModal =
  wrapped Fltk.setModal

setNonModal ::
     Window -- ^
  -> IO ()
setNonModal =
  wrapped Fltk.setNonModal

setNotResizable ::
     Window -- ^
  -> IO ()
setNotResizable =
  wrapped Fltk.setNotResizable

setOutput ::
     Window -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setOverride ::
     Window -- ^
  -> IO ()
setOverride =
  wrapped Fltk.setOverride

setParent ::
     IsGroup group
  => Window -- ^
  -> Maybe group -- ^
  -> IO ()
setParent box = \case
  Nothing ->
    wrapped Fltk.setParent box (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroup group (\ref -> wrapped Fltk.setParent box (Just ref))

setResizable ::
     IsWidget widget
  => Window -- ^
  -> Maybe widget -- ^
  -> IO ()
setResizable window = \case
  Nothing ->
    wrapped Fltk.setResizable window (Nothing @(Fltk.Ref Fltk.WidgetBase))
  Just widget ->
    asWidget widget (\ref -> wrapped Fltk.setResizable window (Just ref))

setSelectionColor ::
     Window -- ^
  -> Fltk.Color -- ^
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

setTooltip ::
     Window -- ^
  -> Text -- ^
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

setTooltipWindow ::
     Window -- ^
  -> IO ()
setTooltipWindow =
  wrapped Fltk.setTooltipWindow

setType ::
     Window -- ^
  -> Fltk.WindowType -- ^
  -> IO ()
setType =
  wrapped Fltk.setType

setVisible ::
     Window -- ^
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Window -- ^
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Window -- ^
  -> [Fltk.When] -- ^
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

setXclass ::
     Window -- ^
  -> Text -- ^
  -> IO ()
setXclass =
  wrapped Fltk.setXclass

shown ::
     Window -- ^
  -> IO Bool
shown =
  wrapped Fltk.shown

showWidget ::
     Window -- ^
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

sizeRange ::
     Window -- ^
  -> Fltk.Size -- ^
  -> IO ()
sizeRange =
  wrapped Fltk.sizeRange

sizeRangeWithArgs ::
     Window -- ^
  -> Fltk.Size -- ^
  -> Fltk.OptionalSizeRangeArgs -- ^
  -> IO ()
sizeRangeWithArgs =
  wrapped Fltk.sizeRangeWithArgs

takeFocus ::
     Window -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Window -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents

updateChild ::
     IsWidget widget
  => Window -- ^
  -> widget -- ^
  -> IO ()
updateChild window widget =
  asWidget widget (wrapped Fltk.updateChild window)

waitForExpose ::
     Window -- ^
  -> IO ()
waitForExpose =
  wrapped Fltk.waitForExpose

within ::
     Window -- ^
  -> IO a -- ^
  -> IO a
within =
  wrapped Fltk.within
