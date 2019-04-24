module Fltk.Button
  ( -- * Button
    Button
  , Style(..)
  , new
    -- ** Read-write properties
  , active
  , align
  , box
  , changed
  , color
  , damage
  , deimage
  , downBox
  , downColor
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
  , shortcut
  , tooltip
  , type_
  , value
  , visible
  , visibleFocus
  , when
    -- ** Read-only queries
  , activeR
  , callback
  , contains
  , height
  , inside
  , rectangle
  , topWindow
  , topWindowOffset
  , takesEvents
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
  , hasCallback
  , measureLabel
  , redraw
  , redrawLabel
  , resize
  , setCallback
  , setColorWithBgSel
  , setDamageInside
  , setonly
  , takeFocus
  ) where

import Fltk.Internal.Types (Button(..), Group(..), Image(..), Widget(..),
                            Window(..))

import qualified Fltk.Internal as Internal

import Data.Coerce   (coerce)
import Data.Functor  (void)
import Data.StateVar (StateVar, makeStateVar)
import Data.Text     (Text)
import Foreign.Ptr   (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Button           as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.CheckButton      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.LightButton      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.RadioLightButton as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.RepeatButton     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.ReturnButton     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.RoundButton      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.ToggleButton     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget           as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch              as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations       as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types              as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy             as Fltk


data Style
  = Check
  | Light
  | Plain
  | RadioLight
  | Repeat
  | Return
  | Round
  | Toggle

wrapped ::
     (Fltk.Ref Fltk.ButtonBase -> a)
  -> Button
  -> a
wrapped =
  coerce

new ::
     Style -- ^
  -> Fltk.Rectangle -- ^
  -> Text -- ^
  -> IO Button
new style bounds label =
  case style of
    Check      -> go Fltk.checkButtonNew
    Light      -> go Fltk.lightButtonNew
    Plain      -> go Fltk.buttonNew
    RadioLight -> go Fltk.radioLightButtonNew
    Repeat     -> go Fltk.repeatButtonNew
    Return     -> go Fltk.returnButtonNew
    Round      -> go Fltk.roundButtonNew
    Toggle     -> go Fltk.toggleButtonNew

  where
    go ::
         Fltk.Parent button Fltk.ButtonBase
      => (Fltk.Rectangle -> Maybe Text -> IO (Fltk.Ref button))
      -> IO Button
    go f =
      Button . Fltk.safeCast <$> f bounds (Just label)


--------------------------------------------------------------------------------
-- Read-write properties
--------------------------------------------------------------------------------

prop ::
     (Fltk.Ref Fltk.ButtonBase -> IO a)
  -> (Fltk.Ref Fltk.ButtonBase -> a -> IO ())
  -> Button
  -> StateVar a
prop getter setter button =
  makeStateVar (wrapped getter button) (wrapped setter button)

active ::
     Button -- ^
  -> StateVar Bool
active =
  wrapped Internal.active

align ::
     Button -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     Button -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     Button -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

color ::
     Button -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

damage ::
     Button -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     Button -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

downBox ::
     Button -- ^
  -> StateVar Fltk.Boxtype
downBox =
  prop Fltk.getDownBox Fltk.setDownBox

downColor ::
     Button -- ^
  -> StateVar Fltk.Color
downColor =
  prop Fltk.getDownColor Fltk.setDownColor

flags ::
     Button -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

image ::
     Button -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

label ::
     Button -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelColor ::
     Button -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Internal.labelColor

labelFont ::
     Button -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Internal.labelFont

labelSize ::
     Button -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Internal.labelSize

labelType ::
     Button -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Internal.labelType

output ::
     Button -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     Button -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

selectionColor ::
     Button -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

shortcut ::
     Button -- ^
  -> StateVar (Maybe Fltk.ShortcutKeySequence)
shortcut =
  wrapped Internal.shortcut

tooltip ::
     Button -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     Button -- ^
  -> StateVar Fltk.ButtonType
type_ =
  prop Fltk.getType_ Fltk.setType

value ::
     Button -- ^
  -> StateVar Bool
value =
  prop Fltk.getValue (\b -> void . Fltk.setValue b)

visible ::
     Button -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     Button -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     Button -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when


--------------------------------------------------------------------------------
-- Read-only queries
--------------------------------------------------------------------------------

activeR ::
     Button -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

callback ::
     Button -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

contains ::
     Button -- ^
  -> Widget -- ^
  -> IO Bool
contains button widget =
  wrapped Fltk.contains button (unWidget widget)

height ::
     Button -- ^
  -> IO Fltk.Height
height =
  wrapped Fltk.getH

rectangle ::
     Button -- ^
  -> IO Fltk.Rectangle
rectangle =
  wrapped Fltk.getRectangle

takesEvents ::
     Button -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

topWindow ::
     Button -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     Button -- ^
  -> IO Fltk.Position
topWindowOffset =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     Button -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

width ::
     Button -- ^
  -> IO Fltk.Width
width =
  wrapped Fltk.getW

window ::
     Button -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)

x ::
     Button -- ^
  -> IO Fltk.X
x =
  wrapped Fltk.getX

y ::
     Button -- ^
  -> IO Fltk.Y
y =
  wrapped Fltk.getY


--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

copyTooltip ::
     Button -- ^
  -> Text -- ^
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

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

inside ::
     Button -- ^
  -> Widget -- ^
  -> IO Bool
inside button widget =
  wrapped Fltk.inside button (unWidget widget)

measureLabel ::
     Button -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

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

setCallback ::
     Button -- ^
  -> (Button -> IO ()) -- ^
  -> IO ()
setCallback button callback =
  wrapped Fltk.setCallback button (coerce callback)

setColorWithBgSel ::
     Button -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamageInside ::
     Button -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setonly ::
     Button -- ^
  -> IO ()
setonly =
  wrapped Fltk.setonly

takeFocus ::
     Button -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus
