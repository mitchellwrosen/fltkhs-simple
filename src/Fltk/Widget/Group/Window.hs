module Fltk.Widget.Group.Window
  ( -- * Window
    Window
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

import Fltk.Internal.Types (Group(..), Image(..), LabelStyle, Widget(..),
                            Window(..))

import qualified Fltk.Internal as Internal

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
-- Read-write properties
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
  wrapped Internal.active

align ::
     Window -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     Window -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     Window -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

clipChildren ::
     Window -- ^
  -> StateVar Bool
clipChildren =
  wrapped Internal.clipChildren

color ::
     Window -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

damage ::
     Window -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     Window -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

flags ::
     Window -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

icon ::
     Window -- ^
  -> StateVar (Maybe Image)
icon =
  wrapped Internal.icon

iconLabel ::
     Window -- ^
  -> StateVar Text
iconLabel =
  wrapped Internal.iconLabel

image ::
     Window -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

label ::
     Window -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelStyle ::
     Window -- ^
  -> StateVar LabelStyle
labelStyle =
  wrapped Internal.labelStyle

output ::
     Window -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     Window -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

selectionColor ::
     Window -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

shown ::
     Window -- ^
  -> IO Bool
shown =
  wrapped Fltk.shown

size ::
     Window -- ^
  -> StateVar Fltk.Rectangle
size =
  wrapped Internal.size

tooltip ::
     Window -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     Window -- ^
  -> StateVar Fltk.WindowType
type_ =
  prop Fltk.getType_ Fltk.setType

visible ::
     Window -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     Window -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     Window -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when

xclass ::
     Window -- ^
  -> StateVar Text
xclass =
  wrapped Internal.xclass


--------------------------------------------------------------------------------
-- Read-only queries
--------------------------------------------------------------------------------

activeR ::
     Window -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

array ::
     Window -- ^
  -> IO [Widget]
array =
  coerce (wrapped Fltk.getArray)

border ::
     Window -- ^
  -> IO Bool
border =
  wrapped Fltk.getBorder

callback ::
     Window -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

child ::
     Window -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
child =
  coerce (wrapped Fltk.getChild)

children ::
     Window -- ^
  -> IO Int
children =
  wrapped Fltk.children

contains ::
     Window -- ^
  -> Widget -- ^
  -> IO Bool
contains window widget =
  wrapped Fltk.contains window (unWidget widget)

decoratedH ::
     Window -- ^
  -> IO Int
decoratedH =
  wrapped Fltk.getDecoratedH

decoratedW ::
     Window -- ^
  -> IO Int
decoratedW =
  wrapped Fltk.getDecoratedW

find ::
     Window -- ^
  -> Widget -- ^
  -> IO Fltk.AtIndex
find window widget =
  wrapped Fltk.find window (unWidget widget)

hasCallback ::
     Window -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

inside ::
     Window -- ^
  -> Widget -- ^
  -> IO Bool
inside window widget =
  wrapped Fltk.inside window (unWidget widget)

menuWindow ::
     Window -- ^
  -> IO Bool
menuWindow =
  wrapped Fltk.getMenuWindow

modal ::
     Window -- ^
  -> IO Bool
modal =
  wrapped Fltk.getModal

nonModal ::
     Window -- ^
  -> IO Bool
nonModal =
  wrapped Fltk.nonModal

override ::
     Window -- ^
  -> IO Bool
override =
  wrapped Fltk.getOverride

takesEvents ::
     Window -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

tooltipWindow ::
     Window -- ^
  -> IO Bool
tooltipWindow =
  wrapped Fltk.getTooltipWindow

topWindow ::
     Window -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     Window -- ^
  -> IO Fltk.Position
topWindowOffset =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     Window -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

window ::
     Window -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)

xRoot ::
     Window -- ^
  -> IO Int
xRoot =
  wrapped Fltk.getXRoot

yRoot ::
     Window -- ^
  -> IO Int
yRoot =
  wrapped Fltk.getYRoot


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

add ::
     Window -- ^
  -> Widget -- ^
  -> IO ()
add window widget =
  wrapped Fltk.add window (unWidget widget)

addResizable ::
     Window -- ^
  -> Widget -- ^
  -> IO ()
addResizable window widget =
  wrapped Fltk.addResizable window (unWidget widget)

begin ::
     Window -- ^
  -> IO ()
begin =
  wrapped Fltk.begin

clear ::
     Window -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

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

end ::
     Window -- ^
  -> IO ()
end =
  wrapped Fltk.end

flush ::
     Window -- ^
  -> IO ()
flush =
  wrapped Fltk.flush

focus ::
     Window -- ^
  -> Widget -- ^
  -> IO ()
focus window widget =
  wrapped Fltk.focus window (unWidget widget)

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

-- getResizable ::
--      Window -- ^
--   -> IO (Maybe Widget)
-- getResizable =
--   coerce (wrapped Fltk.getResizable)

handle ::
     Window -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

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
     Window -- ^
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
     Window -- ^
  -> Widget -- ^
  -> IO ()
removeWidget window widget =
  wrapped Fltk.removeWidget window (unWidget widget)

setBorder ::
     Window -- ^
  -> Bool -- ^
  -> IO ()
setBorder =
  wrapped Fltk.setBorder

setCallback ::
     Window -- ^
  -> IO () -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (const callback)

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

setLabelWithIconlabel ::
     Window -- ^
  -> Text -- ^
  -> Text -- ^
  -> IO ()
setLabelWithIconlabel =
  wrapped Fltk.setLabelWithIconlabel

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

setTooltipWindow ::
     Window -- ^
  -> IO ()
setTooltipWindow =
  wrapped Fltk.setTooltipWindow

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

updateChild ::
     Window -- ^
  -> Widget -- ^
  -> IO ()
updateChild window widget =
  wrapped Fltk.updateChild window (unWidget widget)

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
