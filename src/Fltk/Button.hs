module Fltk.Button
  ( -- * Button
    Button
  , Style(..)
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
  , image
  , label
  , labelColor
  , labelFont
  , labelSize
  , labelType
  , output
  , selectionColor
  , tooltip
  , type_
  , visible
  , visibleFocus
  , when
    -- ** Functions
  , activeR
  , contains
  , copyTooltip
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , getCallback
  , getDownBox
  , getDownColor
  , getH
  , getParent
  , getRectangle
  , getShortcut
  , getTopWindow
  , getTopWindowOffset
  , getValue
  , getVisibleR
  , getW
  , getWindow
  , getX
  , getY
  , handle
  , hasCallback
  , inside
  , measureLabel
  , redraw
  , redrawLabel
  , resize
  , setCallback
  , setColorWithBgSel
  , setDamageInside
  , setDownBox
  , setDownColor
  , setonly
  , setParent
  , setShortcut
  , setValue
  , takeFocus
  , takesevents
  ) where

import Fltk.Internal.Types (Button(..), Group(..), Image(..), Widget(..),
                            Window(..))

import qualified Fltk.Internal.Widget as Widget

import Data.Coerce   (coerce)
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
-- Properties
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
  wrapped Widget.active

align ::
     Button -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Widget.align

box ::
     Button -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Widget.box

changed ::
     Button -- ^
  -> StateVar Bool
changed =
  wrapped Widget.changed

color ::
     Button -- ^
  -> StateVar Fltk.Color
color =
  wrapped Widget.color

damage ::
     Button -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Widget.damage

deimage ::
     Button -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Widget.deimage

flags ::
     Button -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Widget.flags

image ::
     Button -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Widget.image

label ::
     Button -- ^
  -> StateVar Text
label =
  wrapped Widget.label

labelColor ::
     Button -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Widget.labelColor

labelFont ::
     Button -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Widget.labelFont

labelSize ::
     Button -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Widget.labelSize

labelType ::
     Button -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Widget.labelType

output ::
     Button -- ^
  -> StateVar Bool
output =
  wrapped Widget.output

selectionColor ::
     Button -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Widget.selectionColor

tooltip ::
     Button -- ^
  -> StateVar Text
tooltip =
  wrapped Widget.tooltip

type_ ::
     Button -- ^
  -> StateVar Fltk.ButtonType
type_ =
  prop Fltk.getType_ Fltk.setType

visible ::
     Button -- ^
  -> StateVar Bool
visible =
  wrapped Widget.visible

visibleFocus ::
     Button -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Widget.visibleFocus

when ::
     Button -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Widget.when


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

activeR ::
     Button -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

contains ::
     Button -- ^
  -> Widget -- ^
  -> IO Bool
contains button widget =
  wrapped Fltk.contains button (unWidget widget)

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

getCallback ::
     Button -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

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

getShortcut ::
     Button -- ^
  -> IO (Maybe Fltk.ShortcutKeySequence)
getShortcut =
  wrapped Fltk.getShortcut

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

getValue ::
     Button -- ^
  -> IO Bool
getValue =
  wrapped Fltk.getValue

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

setonly ::
     Button -- ^
  -> IO ()
setonly =
  wrapped Fltk.setonly

setParent ::
     Button -- ^
  -> Maybe Group -- ^
  -> IO ()
setParent button group =
  wrapped Fltk.setParent button (coerce group :: Maybe (Fltk.Ref Fltk.GroupBase))

setShortcut ::
     Button -- ^
  -> Fltk.ShortcutKeySequence -- ^
  -> IO ()
setShortcut =
  wrapped Fltk.setShortcut

setValue ::
     Button -- ^
  -> Bool -- ^
  -> IO Bool
setValue =
  wrapped Fltk.setValue

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
