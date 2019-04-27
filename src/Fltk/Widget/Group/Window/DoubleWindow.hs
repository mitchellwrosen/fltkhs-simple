module Fltk.Widget.Group.Window.DoubleWindow
  ( -- * DoubleWindow
    DoubleWindow
  , new
    -- ** Read-write properties
  , active
  , align
  , box
  , changed
  , clipChildren
  , color
  , damage
  , deimage
  , flags
  , icon
  , iconLabel
  , image
  , label
  , labelStyle
  , output
  , parent
  -- , resizable
  , selectionColor
  , size
  , tooltip
  , type_
  , visible
  , visibleFocus
  , when
  , xclass
    -- ** Read-only queries
  , activeR
  , array
  , border
  , callback
  , child
  , children
  , contains
  , decoratedH
  , decoratedW
  , find
  , hasCallback
  , inside
  , menuWindow
  , modal
  , nonModal
  , override
  , shown
  , takesEvents
  , tooltipWindow
  , topWindow
  , topWindowOffset
  , visibleR
  , window
  , xRoot
  , yRoot
    -- ** Functions
  , add
  , addResizable
  , begin
  , clear
  , copyLabel
  , copyTooltip
  , ddfdesignKludge
  , destroy
  , doCallback
  , end
  , flush
  , focus
  , freePosition
  , fullscreenOff
  , handle
  , hotSpot
  , iconize
  , initSizes
  , insert
  , makeCurrent
  , makeFullscreen
  , measureLabel
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , setBorder
  , setCallback
  , setColorWithBgSel
  , setCursor
  , setCursorWithFgBg
  , setDamageInside
  , setDefaultCursor
  , setDefaultCursorWithFgBg
  , setLabelWithIconlabel
  , setModal
  , setNonModal
  , setNotResizable
  , setOverride
  , setTooltipWindow
  , sizeRange
  , sizeRangeWithArgs
  , takeFocus
  , updateChild
  , waitForExpose
  , within
  ) where

import Fltk.Internal.Types (DoubleWindow(..), Group(..), Image(..), LabelStyle,
                            Widget(..), Window(..))

import qualified Fltk.Internal as Internal

import Data.Coerce                                 (coerce)
import Data.StateVar                               (StateVar, makeStateVar)
import Data.Text                                   (Text)
import Foreign.Ptr                                 (FunPtr)
import Graphics.UI.FLTK.LowLevel.Base.DoubleWindow ()
import Graphics.UI.FLTK.LowLevel.Base.Group        ()

import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Window     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


wrapped ::
     (Fltk.Ref Fltk.DoubleWindowBase -> a)
  -> DoubleWindow
  -> a
wrapped =
  coerce


new ::
     Fltk.Size -- ^
  -> Maybe Fltk.Position -- ^
  -> Text -- ^
  -> IO DoubleWindow
new size pos title =
  coerce (Fltk.windowNew size pos (Just title))


--------------------------------------------------------------------------------
-- Read-write properties
--------------------------------------------------------------------------------

prop ::
     (Fltk.Ref Fltk.DoubleWindowBase -> IO a)
  -> (Fltk.Ref Fltk.DoubleWindowBase -> a -> IO ())
  -> DoubleWindow
  -> StateVar a
prop getter setter window =
  makeStateVar (wrapped getter window) (wrapped setter window)

active ::
     DoubleWindow -- ^
  -> StateVar Bool
active =
  wrapped Internal.active

align ::
     DoubleWindow -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     DoubleWindow -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     DoubleWindow -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

clipChildren ::
     DoubleWindow -- ^
  -> StateVar Bool
clipChildren =
  wrapped Internal.clipChildren

color ::
     DoubleWindow -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

damage ::
     DoubleWindow -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     DoubleWindow -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

flags ::
     DoubleWindow -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

icon ::
     DoubleWindow -- ^
  -> StateVar (Maybe Image)
icon =
  wrapped Internal.icon

iconLabel ::
     DoubleWindow -- ^
  -> StateVar Text
iconLabel =
  wrapped Internal.iconLabel

image ::
     DoubleWindow -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

label ::
     DoubleWindow -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelStyle ::
     DoubleWindow -- ^
  -> StateVar LabelStyle
labelStyle =
  wrapped Internal.labelStyle

output ::
     DoubleWindow -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     DoubleWindow -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

selectionColor ::
     DoubleWindow -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

shown ::
     DoubleWindow -- ^
  -> IO Bool
shown =
  wrapped Fltk.shown

size ::
     DoubleWindow -- ^
  -> StateVar Fltk.Rectangle
size =
  wrapped Internal.size

tooltip ::
     DoubleWindow -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     DoubleWindow -- ^
  -> StateVar Fltk.WindowType
type_ =
  prop Fltk.getType_ Fltk.setType

visible ::
     DoubleWindow -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     DoubleWindow -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     DoubleWindow -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when

xclass ::
     DoubleWindow -- ^
  -> StateVar Text
xclass =
  wrapped Internal.xclass


--------------------------------------------------------------------------------
-- Read-only queries
--------------------------------------------------------------------------------

activeR ::
     DoubleWindow -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

array ::
     DoubleWindow -- ^
  -> IO [Widget]
array =
  coerce (wrapped Fltk.getArray)

border ::
     DoubleWindow -- ^
  -> IO Bool
border =
  wrapped Fltk.getBorder

callback ::
     DoubleWindow -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

child ::
     DoubleWindow -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
child =
  coerce (wrapped Fltk.getChild)

children ::
     DoubleWindow -- ^
  -> IO Int
children =
  wrapped Fltk.children

contains ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO Bool
contains window widget =
  wrapped Fltk.contains window (unWidget widget)

decoratedH ::
     DoubleWindow -- ^
  -> IO Int
decoratedH =
  wrapped Fltk.getDecoratedH

decoratedW ::
     DoubleWindow -- ^
  -> IO Int
decoratedW =
  wrapped Fltk.getDecoratedW

find ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO Fltk.AtIndex
find window widget =
  wrapped Fltk.find window (unWidget widget)

hasCallback ::
     DoubleWindow -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

inside ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO Bool
inside window widget =
  wrapped Fltk.inside window (unWidget widget)

menuWindow ::
     DoubleWindow -- ^
  -> IO Bool
menuWindow =
  wrapped Fltk.getMenuWindow

modal ::
     DoubleWindow -- ^
  -> IO Bool
modal =
  wrapped Fltk.getModal

nonModal ::
     DoubleWindow -- ^
  -> IO Bool
nonModal =
  wrapped Fltk.nonModal

override ::
     DoubleWindow -- ^
  -> IO Bool
override =
  wrapped Fltk.getOverride

takesEvents ::
     DoubleWindow -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

tooltipWindow ::
     DoubleWindow -- ^
  -> IO Bool
tooltipWindow =
  wrapped Fltk.getTooltipWindow

topWindow ::
     DoubleWindow -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     DoubleWindow -- ^
  -> IO Fltk.Position
topWindowOffset =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     DoubleWindow -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

window ::
     DoubleWindow -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)

xRoot ::
     DoubleWindow -- ^
  -> IO Int
xRoot =
  wrapped Fltk.getXRoot

yRoot ::
     DoubleWindow -- ^
  -> IO Int
yRoot =
  wrapped Fltk.getYRoot


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

add ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO ()
add window widget =
  wrapped Fltk.add window (unWidget widget)

addResizable ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO ()
addResizable window widget =
  wrapped Fltk.addResizable window (unWidget widget)

begin ::
     DoubleWindow -- ^
  -> IO ()
begin =
  wrapped Fltk.begin

clear ::
     DoubleWindow -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

copyLabel ::
     DoubleWindow -- ^
  -> Text -- ^
  -> IO ()
copyLabel =
  wrapped Fltk.copyLabel

copyTooltip ::
     DoubleWindow -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

ddfdesignKludge ::
     DoubleWindow -- ^
  -> IO (Maybe Widget)
ddfdesignKludge =
  coerce (wrapped Fltk.ddfdesignKludge)

destroy ::
     DoubleWindow -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     DoubleWindow -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

end ::
     DoubleWindow -- ^
  -> IO ()
end =
  wrapped Fltk.end

flush ::
     DoubleWindow -- ^
  -> IO ()
flush =
  wrapped Fltk.flush

focus ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO ()
focus window widget =
  wrapped Fltk.focus window (unWidget widget)

freePosition ::
     DoubleWindow -- ^
  -> IO ()
freePosition =
  wrapped Fltk.freePosition

fullscreenOff ::
     DoubleWindow -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
fullscreenOff =
  wrapped Fltk.fullscreenOff

-- getResizable ::
--      DoubleWindow -- ^
--   -> IO (Maybe Widget)
-- getResizable =
--   coerce (wrapped Fltk.getResizable)

handle ::
     DoubleWindow -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hotSpot ::
     DoubleWindow -- ^
  -> Fltk.PositionSpec -- ^
  -> Maybe Bool -- ^
  -> IO ()
hotSpot =
  wrapped Fltk.hotSpot

iconize ::
     DoubleWindow -- ^
  -> IO ()
iconize =
  wrapped Fltk.iconize

initSizes ::
     DoubleWindow -- ^
  -> IO ()
initSizes =
  wrapped Fltk.initSizes

insert ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
insert window widget =
  wrapped Fltk.insert window (unWidget widget)

-- insertBefore ::
--      IsWidget widget
--   => Window Group
--   -> widget
--   -> Ref b
--   -> IO ()

makeCurrent ::
     DoubleWindow -- ^
  -> IO ()
makeCurrent =
  wrapped Fltk.makeCurrent

makeFullscreen ::
     DoubleWindow -- ^
  -> IO ()
makeFullscreen =
  wrapped Fltk.makeFullscreen

measureLabel ::
     DoubleWindow -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

redraw ::
     DoubleWindow -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     DoubleWindow -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

removeIndex ::
     DoubleWindow -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
removeIndex =
  wrapped Fltk.removeIndex

removeWidget ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO ()
removeWidget window widget =
  wrapped Fltk.removeWidget window (unWidget widget)

setBorder ::
     DoubleWindow -- ^
  -> Bool -- ^
  -> IO ()
setBorder =
  wrapped Fltk.setBorder

setCallback ::
     DoubleWindow -- ^
  -> IO () -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (const callback)

setColorWithBgSel ::
     DoubleWindow -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setCursor ::
     DoubleWindow -- ^
  -> Fltk.Cursor -- ^
  -> IO ()
setCursor =
  wrapped Fltk.setCursor

setCursorWithFgBg ::
     DoubleWindow -- ^
  -> Fltk.Cursor -- ^
  -> (Maybe Fltk.Color, Maybe Fltk.Color) -- ^
  -> IO ()
setCursorWithFgBg =
  wrapped Fltk.setCursorWithFgBg

setDamageInside ::
     DoubleWindow -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDefaultCursor ::
     DoubleWindow -- ^
  -> Fltk.CursorType -- ^
  -> IO ()
setDefaultCursor =
  wrapped Fltk.setDefaultCursor

setDefaultCursorWithFgBg ::
     DoubleWindow -- ^
  -> Fltk.CursorType -- ^
  -> (Maybe Fltk.Color, Maybe Fltk.Color) -- ^
  -> IO ()
setDefaultCursorWithFgBg =
  wrapped Fltk.setDefaultCursorWithFgBg

setLabelWithIconlabel ::
     DoubleWindow -- ^
  -> Text -- ^
  -> Text -- ^
  -> IO ()
setLabelWithIconlabel =
  wrapped Fltk.setLabelWithIconlabel

setModal ::
     DoubleWindow -- ^
  -> IO ()
setModal =
  wrapped Fltk.setModal

setNonModal ::
     DoubleWindow -- ^
  -> IO ()
setNonModal =
  wrapped Fltk.setNonModal

setNotResizable ::
     DoubleWindow -- ^
  -> IO ()
setNotResizable =
  wrapped Fltk.setNotResizable

setOverride ::
     DoubleWindow -- ^
  -> IO ()
setOverride =
  wrapped Fltk.setOverride

setTooltipWindow ::
     DoubleWindow -- ^
  -> IO ()
setTooltipWindow =
  wrapped Fltk.setTooltipWindow

sizeRange ::
     DoubleWindow -- ^
  -> Fltk.Size -- ^
  -> IO ()
sizeRange =
  wrapped Fltk.sizeRange

sizeRangeWithArgs ::
     DoubleWindow -- ^
  -> Fltk.Size -- ^
  -> Fltk.OptionalSizeRangeArgs -- ^
  -> IO ()
sizeRangeWithArgs =
  wrapped Fltk.sizeRangeWithArgs

takeFocus ::
     DoubleWindow -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

updateChild ::
     DoubleWindow -- ^
  -> Widget -- ^
  -> IO ()
updateChild window widget =
  wrapped Fltk.updateChild window (unWidget widget)

waitForExpose ::
     DoubleWindow -- ^
  -> IO ()
waitForExpose =
  wrapped Fltk.waitForExpose

within ::
     DoubleWindow -- ^
  -> IO a -- ^
  -> IO a
within =
  wrapped Fltk.within
