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

import qualified Fltk.Internal as Internal

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
  wrapped Internal.active

align ::
     Box -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     Box -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     Box -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

color ::
     Box -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

damage ::
     Box -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     Box -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

flags ::
     Box -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

image ::
     Box -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

label ::
     Box -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelColor ::
     Box -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Internal.labelColor

labelFont ::
     Box -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Internal.labelFont

labelSize ::
     Box -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Internal.labelSize

labelType ::
     Box -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Internal.labelType

output ::
     Box -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     Box -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

selectionColor ::
     Box -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

tooltip ::
     Box -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     Box -- ^
  -> StateVar Word8
type_ =
  wrapped Internal.type_

visible ::
     Box -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     Box -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     Box -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when


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
