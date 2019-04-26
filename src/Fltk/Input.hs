module Fltk.Input
  ( -- * Input
    Input
  , new
    -- ** Read-write properties
  , active
  , align
  , box
  , changed
  , color
  , cursorColor
  , damage
  , deimage
  , flags
  , image
  , inputType
  , label
  , labelColor
  , labelFont
  , labelSize
  , labelType
  , mark
  , maximumSize
  , output
  , parent
  , position
  , readonly
  , selectionColor
  , shortcut
  , tabNav
  , textColor
  , textFont
  , textSize
  , tooltip
  , type_
  , value
  , visible
  , visibleFocus
  , when
  , wrap
    -- ** Read-only queries
  , activeR
  , callback
  , contains
  , hasCallback
  , height
  , index
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
  , copy
  , copyCuts
  , copyTooltip
  , cut
  , cutFromCursor
  , cutRange
  , destroy
  , doCallback
  , drawText
  , handle
  , insert
  , insertWithLength
  , measureLabel
  , redraw
  , redrawLabel
  , replace
  , resize
  , setCallback
  , setColorWithBgSel
  , setDamageInside
  , setSize
  , takeFocus
  , undo
  ) where

import Fltk.Internal.Types (Group(..), Image(..), Input(..), Widget(..),
                            Window(..))

import qualified Fltk.Internal as Internal

import Data.Coerce   (coerce)
import Data.StateVar (StateVar)
import Data.Text     (Text)
import Data.Word     (Word8)
import Foreign.Ptr   (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Input      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


new ::
     Fltk.Rectangle -- ^
  -> Text -- ^
  -> Maybe Fltk.FlInputType
  -> IO Input
new bounds label typ =
  Input . Fltk.safeCast <$> Fltk.inputNew bounds (Just label) typ

wrapped ::
     (Fltk.Ref Fltk.InputBase -> a)
  -> Input
  -> a
wrapped =
  coerce


--------------------------------------------------------------------------------
-- Read-write properties
--------------------------------------------------------------------------------

active ::
     Input -- ^
  -> StateVar Bool
active =
  wrapped Internal.active

align ::
     Input -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     Input -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     Input -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

color ::
     Input -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

cursorColor ::
     Input -- ^
  -> StateVar Fltk.Color
cursorColor =
  wrapped Internal.cursorColor

damage ::
     Input -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     Input -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

flags ::
     Input -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

image ::
     Input -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

inputType ::
     Input -- ^
  -> StateVar Fltk.FlInputType
inputType =
  wrapped Internal.inputType

label ::
     Input -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelColor ::
     Input -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Internal.labelColor

labelFont ::
     Input -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Internal.labelFont

labelSize ::
     Input -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Internal.labelSize

labelType ::
     Input -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Internal.labelType

mark ::
     Input -- ^
  -> StateVar Int
mark =
  wrapped Internal.mark

maximumSize ::
     Input -- ^
  -> StateVar Int
maximumSize =
  wrapped Internal.maximumSize

output ::
     Input -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     Input -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

position ::
     Input -- ^
  -> StateVar Int
position =
  wrapped Internal.position

readonly ::
     Input -- ^
  -> StateVar Bool
readonly =
  wrapped Internal.readonly

selectionColor ::
     Input -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

shortcut ::
     Input -- ^
  -> StateVar (Maybe Fltk.ShortcutKeySequence)
shortcut =
  wrapped Internal.shortcut

tabNav ::
     Input -- ^
  -> StateVar Bool
tabNav =
  wrapped Internal.tabNav

textColor ::
     Input -- ^
  -> StateVar Fltk.Color
textColor =
  wrapped Internal.textColor

textFont ::
     Input -- ^
  -> StateVar Fltk.Font
textFont =
  wrapped Internal.textFont

textSize ::
     Input -- ^
  -> StateVar Fltk.FontSize
textSize =
  wrapped Internal.textSize

tooltip ::
     Input -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     Input -- ^
  -> StateVar Word8
type_ =
  wrapped Internal.type_

value ::
     Input -- ^
  -> StateVar Text
value =
  wrapped Internal.value

visible ::
     Input -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     Input -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     Input -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when

wrap ::
     Input -- ^
  -> StateVar Bool
wrap =
  wrapped Internal.wrap


--------------------------------------------------------------------------------
-- Read-only queries
--------------------------------------------------------------------------------

activeR ::
     Input -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

contains ::
     Input -- ^
  -> Widget -- ^
  -> IO Bool
contains input widget =
  wrapped Fltk.contains input (unWidget widget)

callback ::
     Input -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

hasCallback ::
     Input -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

height ::
     Input -- ^
  -> IO Fltk.Height
height =
  wrapped Fltk.getH

index ::
     Input -- ^
  -> Fltk.AtIndex -- ^
  -> IO Char
index =
  wrapped Fltk.index

inside ::
     Input -- ^
  -> Widget -- ^
  -> IO Bool
inside input widget =
  wrapped Fltk.inside input (unWidget widget)

rectangle ::
     Input -- ^
  -> IO Fltk.Rectangle
rectangle =
  wrapped Fltk.getRectangle

takesEvents ::
     Input -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

topWindow ::
     Input -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     Input -- ^
  -> IO Fltk.Position
topWindowOffset =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     Input -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

width ::
     Input -- ^
  -> IO Fltk.Width
width =
  wrapped Fltk.getW

window ::
     Input -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)

x ::
     Input -- ^
  -> IO Fltk.X
x =
  wrapped Fltk.getX

y ::
     Input -- ^
  -> IO Fltk.Y
y =
  wrapped Fltk.getY


--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

-- | Put the current selection into the clipboard.
copy ::
     Input -- ^
  -> Fltk.Clipboard -- ^
  -> IO (Either Fltk.NoChange ())
copy =
  wrapped Fltk.copy

-- | Copies the yank buffer to the clipboard.
copyCuts ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
copyCuts =
  wrapped Fltk.copyCuts

copyTooltip ::
     Input -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

-- | Deletes the current selection.
cut ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
cut =
  wrapped Fltk.cut

cutFromCursor ::
     Input -- ^
  -> Int -- ^
  -> IO (Either Fltk.NoChange ())
cutFromCursor =
  wrapped Fltk.cutFromCursor

cutRange ::
     Input -- ^
  -> Fltk.IndexRange -- ^
  -> IO (Either Fltk.NoChange ())
cutRange =
  wrapped Fltk.cutRange

destroy ::
     Input -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Input -- ^
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

drawText ::
     Input -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
drawText =
  wrapped Fltk.drawText

handle ::
     Input -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

insert ::
     Input -- ^
  -> Text -- ^
  -> IO (Either Fltk.NoChange ())
insert =
  wrapped Fltk.insert

insertWithLength ::
     Input -- ^
  -> Text -- ^
  -> Int -- ^
  -> IO (Either Fltk.NoChange ())
insertWithLength =
  wrapped Fltk.insertWithLength

measureLabel ::
     Input -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

redraw ::
     Input -- ^
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Input -- ^
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

replace ::
     Input -- ^
  -> Fltk.IndexRange -- ^
  -> Text -- ^
  -> IO (Either Fltk.NoChange ())
replace =
  wrapped Fltk.replace

resize ::
     Input -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

setCallback ::
     Input -- ^
  -> (Input -> IO ()) -- ^
  -> IO ()
setCallback input callback =
  wrapped Fltk.setCallback input (coerce callback)

setColorWithBgSel ::
     Input -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamageInside ::
     Input -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setSize ::
     Input -- ^
  -> Fltk.Size -- ^
  -> IO ()
setSize =
  wrapped Fltk.setSize

takeFocus ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

undo ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
undo =
  wrapped Fltk.undo
