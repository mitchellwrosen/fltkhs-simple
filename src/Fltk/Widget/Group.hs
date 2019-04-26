module Fltk.Widget.Group
  ( -- * Group
    Group
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
  , image
  , label
  , labelColor
  , labelFont
  , labelSize
  , labelType
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
    -- ** Read-only queries
  , activeR
  , array
  , callback
  , child
  , children
  , contains
  , find
  , hasCallback
  , inside
  , takesEvents
  , topWindow
  , topWindowOffset
  , visibleR
  , window
    -- ** Effectful functions
  , add
  , addResizable
  , begin
  , clear
  , copyTooltip
  , ddfdesignKludge
  , destroy
  , doCallback
  , end
  , focus
  , handle
  , initSizes
  , insert
  , measureLabel
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , setCallback
  , setColorWithBgSel
  , setDamageInside
  , takeFocus
  , updateChild
  , within
  ) where

import Fltk.Internal.Types (Group(..), Image(..), Widget(..), Window(..))

import qualified Fltk.Internal as Internal

import Data.Coerce   (coerce)
import Data.StateVar (StateVar)
import Data.Text     (Text)
import Data.Word     (Word8)
import Foreign.Ptr   (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Group      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


new ::
     Fltk.Rectangle -- ^
  -> Text -- ^
  -> IO Group
new rect label =
  coerce (Fltk.groupNew rect (Just label))

wrapped ::
     (Fltk.Ref Fltk.GroupBase -> a)
  -> Group
  -> a
wrapped =
  coerce


--------------------------------------------------------------------------------
-- Read-write properties
--------------------------------------------------------------------------------

active ::
     Group -- ^
  -> StateVar Bool
active =
  wrapped Internal.active

align ::
     Group -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Internal.align

box ::
     Group -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Internal.box

changed ::
     Group -- ^
  -> StateVar Bool
changed =
  wrapped Internal.changed

clipChildren ::
     Group -- ^
  -> StateVar Bool
clipChildren =
  wrapped Internal.clipChildren

color ::
     Group -- ^
  -> StateVar Fltk.Color
color =
  wrapped Internal.color

damage ::
     Group -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Internal.damage

deimage ::
     Group -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Internal.deimage

flags ::
     Group -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Internal.flags

image ::
     Group -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Internal.image

label ::
     Group -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelColor ::
     Group -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Internal.labelColor

labelFont ::
     Group -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Internal.labelFont

labelSize ::
     Group -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Internal.labelSize

labelType ::
     Group -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Internal.labelType

output ::
     Group -- ^
  -> StateVar Bool
output =
  wrapped Internal.output

parent ::
     Group -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Internal.parent

{-
https://github.com/deech/fltkhs/issues/119

resizable ::
     Group -- ^
  -> StateVar (Maybe Widget)
resizable =
  wrapped Internal.resizable
-}

selectionColor ::
     Group -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Internal.selectionColor

size ::
     Group -- ^
  -> StateVar Fltk.Rectangle
size =
  wrapped Internal.size

tooltip ::
     Group -- ^
  -> StateVar Text
tooltip =
  wrapped Internal.tooltip

type_ ::
     Group -- ^
  -> StateVar Word8
type_ =
  wrapped Internal.type_

visible ::
     Group -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible

visibleFocus ::
     Group -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Internal.visibleFocus

when ::
     Group -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Internal.when


--------------------------------------------------------------------------------
-- Read-only queries
-------------------------------------------------------------------------------

activeR ::
     Group -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

array ::
     Group -- ^
  -> IO [Widget]
array =
  coerce (wrapped Fltk.getArray)

callback ::
     Group -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
callback =
  wrapped Fltk.getCallback

child ::
     Group -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
child =
  coerce (wrapped Fltk.getChild)

children ::
     Group -- ^
  -> IO Int
children =
  wrapped Fltk.children

contains ::
     Group -- ^
  -> Widget -- ^
  -> IO Bool
contains group widget =
  wrapped Fltk.contains group (unWidget widget)

find ::
     Group -- ^
  -> Widget -- ^
  -> IO Fltk.AtIndex
find group widget =
  wrapped Fltk.find group (unWidget widget)

hasCallback ::
     Group -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

inside ::
     Group -- ^
  -> Widget -- ^
  -> IO Bool
inside group widget =
  wrapped Fltk.inside group (unWidget widget)

takesEvents ::
     Group -- ^
  -> IO Bool
takesEvents =
  wrapped Fltk.takesevents

topWindow ::
     Group -- ^
  -> IO (Maybe Window)
topWindow =
  coerce (wrapped Fltk.getTopWindow)

topWindowOffset ::
     Group -- ^
  -> IO Fltk.Position
topWindowOffset  =
  wrapped Fltk.getTopWindowOffset

visibleR ::
     Group -- ^
  -> IO Bool
visibleR =
  wrapped Fltk.getVisibleR

window ::
     Group -- ^
  -> IO (Maybe Window)
window =
  coerce (wrapped Fltk.getWindow)


--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

add ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
add group widget =
  wrapped Fltk.add group (unWidget widget)

addResizable ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
addResizable group widget =
  wrapped Fltk.addResizable group (unWidget widget)

begin ::
     Group -- ^
  -> IO ()
begin =
  wrapped Fltk.begin

clear ::
     Group -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

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

end ::
     Group -- ^
  -> IO ()
end =
  wrapped Fltk.end

focus ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
focus group widget =
  wrapped Fltk.focus group (unWidget widget)

handle ::
     Group -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

initSizes ::
     Group -- ^
  -> IO ()
initSizes =
  wrapped Fltk.initSizes

insert ::
     Group -- ^
  -> Widget -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
insert group widget =
  wrapped Fltk.insert group (unWidget widget)

-- insertBefore :: (Parent a WidgetBase) => Group -> Ref a -> Ref b -> IO ()
-- insertBefore=
--   wrapped Fltk.insertBefore

measureLabel ::
     Group -- ^
  -> Maybe Fltk.Width -- ^
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

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
     Group -- ^
  -> Widget -- ^
  -> IO ()
removeWidget group widget =
  wrapped Fltk.removeWidget group (unWidget widget)

setCallback ::
     Group -- ^
  -> (Group -> IO ()) -- ^
  -> IO ()
setCallback group callback =
  wrapped Fltk.setCallback group (coerce callback)

setColorWithBgSel ::
     Group -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setDamageInside ::
     Group -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

takeFocus ::
     Group -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

updateChild ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
updateChild group widget =
  wrapped Fltk.updateChild group (unWidget widget)

within ::
     Group -- ^
  -> IO a -- ^
  -> IO a
within =
  wrapped Fltk.within
