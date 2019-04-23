module Fltk.Window
  ( Window
  , IsWindow
  , new
    -- * API
    -- ** Properties
  , active
  , align
  , box
  , changed
  , color
  , damage
  , deimage
  , flags
  , label
  , labelColor
  , labelFont
  , labelSize
  , output
  , selectionColor
  , tooltip
  , type_
  , visible
  , visibleFocus
  , when
    -- ** Functions
  , activeR
  , add
  , addResizable
  , begin
  , children
  , clear
  , clearBorder
  , clipChildren
  , contains
  , copyLabel
  , copyTooltip
  , ddfdesignKludge
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
  , flush
  , focus
  , freePosition
  , fullscreenOff
  , getArray
  , getBorder
  , getCallback
  , getChild
  , getDecoratedH
  , getDecoratedW
  , getH
  , getIcon
  , getIconlabel
  , getImage
  , getLabeltype
  , getMenuWindow
  , getModal
  , getOverride
  , getParent
  , getRectangle
  , getResizable
  , getTooltipWindow
  , getTopWindow
  , getTopWindowOffset
  , getVisibleR
  , getW
  , getWindow
  , getX
  , getXRoot
  , getXclass
  , getY
  , getYRoot
  , handle
  , hasCallback
  , hotSpot
  , iconize
  , initSizes
  , insert
  , inside
  , makeCurrent
  , makeFullscreen
  , measureLabel
  , nonModal
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , resize
  , setBorder
  , setCallback
  , setClipChildren
  , setColorWithBgSel
  , setCursor
  , setCursorWithFgBg
  , setDamageInside
  , setDefaultCursor
  , setDefaultCursorWithFgBg
  , setIcon
  , setIconlabel
  , setImage
  , setLabeltype
  , setLabelWithIconlabel
  , setMenuWindow
  , setModal
  , setNonModal
  , setNotResizable
  , setOverride
  , setParent
  , setResizable
  , setTooltipWindow
  , setXclass
  , shown
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

import qualified Fltk.Internal.Widget as Widget

import Data.Coerce                          (coerce)
import Data.StateVar                        (StateVar, makeStateVar)
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


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop ::
     (Fltk.Ref Fltk.WindowBase -> IO a)
  -> (Fltk.Ref Fltk.WindowBase -> a -> IO ())
  -> Window
  -> StateVar a
prop getter setter window =
  makeStateVar (wrapped getter window) (wrapped setter window)

active ::
     Window -- ^
  -> StateVar Bool
active =
  wrapped Widget.active

align ::
     Window -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Widget.align

box ::
     Window -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Widget.box

changed ::
     Window -- ^
  -> StateVar Bool
changed =
  wrapped Widget.changed

color ::
     Window -- ^
  -> StateVar Fltk.Color
color =
  wrapped Widget.color

damage ::
     Window -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Widget.damage

deimage ::
     Window -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Widget.deimage

flags ::
     Window -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Widget.flags

label ::
     Window -- ^
  -> StateVar Text
label =
  wrapped Widget.label

labelColor ::
     Window -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Widget.labelColor

labelFont ::
     Window -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Widget.labelFont

labelSize ::
     Window -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Widget.labelSize

output ::
     Window -- ^
  -> StateVar Bool
output =
  wrapped Widget.output

selectionColor ::
     Window -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Widget.selectionColor

tooltip ::
     Window -- ^
  -> StateVar Text
tooltip =
  wrapped Widget.tooltip

type_ ::
     Window -- ^
  -> StateVar Fltk.WindowType
type_ =
  prop Fltk.getType_ Fltk.setType

visible ::
     Window -- ^
  -> StateVar Bool
visible =
  wrapped Widget.visible

visibleFocus ::
     Window -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Widget.visibleFocus

when ::
     Window -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Widget.when


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

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

clearBorder ::
     Window -- ^
  -> IO ()
clearBorder =
  wrapped Fltk.clearBorder

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

setBorder ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
setBorder =
  wrapped Fltk.setBorder

setCallback ::
     Window -- ^
  -> (Window -> IO ()) -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (coerce callback)

setClipChildren ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
setClipChildren =
  wrapped Fltk.setClipChildren

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

setTooltipWindow ::
     Window -- ^
  -> IO ()
setTooltipWindow =
  wrapped Fltk.setTooltipWindow

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
