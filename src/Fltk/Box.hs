module Fltk.Box
  ( -- * Box
    Box
  , new
    -- ** Read-write properties
  , active
  , align
  , box
  , changed
  , color
  , damage
  , deimage
  , flags
  , image
  , label
  , labelColor
  , labelFont
  , labelSize
  , labelType
  , output
  , parent
  , selectionColor
  , tooltip
  , type_
  , visible
  , visibleFocus
  , when
    -- ** Read-only queries
  , activeR
  , callback
  , contains
  , hasCallback
  , height
  , inside
  , rectangle
  , takesEvents
  , topWindow
  , topWindowOffset
  , visibleR
  , width
  , window
  , x
  , y
    -- ** Effectful functions
  , copyTooltip
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , handle
  , measureLabel
  , redraw
  , redrawLabel
  , resize
  , setCallback
  , setColorWithBgSel
  , setDamageInside
  , takeFocus
  ) where

import Fltk.Internal.Types (Box(..), Group(..), Image(..), Widget(..),
                            Window(..))

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
-- Read-write properties
--------------------------------------------------------------------------------

active ::
     Box -- ^
  -> StateVar Bool
active =
  wrapped Widget.active

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

changed ::
     Box -- ^
  -> StateVar Bool
changed =
  wrapped Widget.changed

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

deimage ::
     Box -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Widget.deimage

flags ::
     Box -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Widget.flags

image ::
     Box -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Widget.image

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

labelType ::
     Box -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Widget.labelType

output ::
     Box -- ^
  -> StateVar Bool
output =
  wrapped Widget.output

parent ::
     Box -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Widget.parent

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
-- Read-only queries
--------------------------------------------------------------------------------

activeR ::
     Box -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

callback ::
     Box -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

contains ::
     Box -- ^
  -> Widget -- ^
  -> IO Bool
contains box widget =
  wrapped Fltk.contains box (unWidget widget)

hasCallback ::
     Box -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

height ::
     Box -- ^
  -> IO Fltk.Height
height =
  wrapped Fltk.getH

inside ::
     Box -- ^
  -> Widget -- ^
  -> IO Bool
inside box widget =
  wrapped Fltk.inside box (unWidget widget)

rectangle ::
     Box -- ^
  -> IO Fltk.Rectangle
rectangle =
 wrapped Fltk.getRectangle

takesEvents ::
     Box -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

topWindow ::
     Box -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     Box -- ^
  -> IO Fltk.Position
topWindowOffset =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     Box -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

width ::
     Box -- ^
  -> IO Fltk.Width
width =
  wrapped Fltk.getW

window ::
     Box -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)

x ::
     Box -- ^
  -> IO Fltk.X
x =
  wrapped Fltk.getX

y ::
     Box -- ^
  -> IO Fltk.Y
y =
  wrapped Fltk.getY


--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

copyTooltip ::
     Box -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

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

handle ::
     Box -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

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

setCallback ::
     Box -- ^
  -> (Box -> IO ()) -- ^
  -> IO ()
setCallback box callback =
  wrapped Fltk.setCallback box (coerce callback)

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

takeFocus ::
     Box -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus
