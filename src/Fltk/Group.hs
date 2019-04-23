module Fltk.Group
  ( -- * Group
    Group
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
  , parent
  -- , resizable
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
  , clipChildren
  , contains
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
  , focus
  , getArray
  , getCallback
  , getChild
  , getH
  , getRectangle
  , getTopWindow
  , getTopWindowOffset
  , getVisibleR
  , getW
  , getWindow
  , getX
  , getY
  , handle
  , hasCallback
  , initSizes
  , insert
  , inside
  , measureLabel
  , redraw
  , redrawLabel
  , removeIndex
  , removeWidget
  , resize
  , setCallback
  , setClipChildren
  , setColorWithBgSel
  , setDamageInside
  , takeFocus
  , takesevents
  , updateChild
  , within
  ) where

import Fltk.Internal.Types (Group(..), Image(..), Widget(..), Window(..))

-- import qualified Fltk.Internal.Group  as Group
import qualified Fltk.Internal.Widget as Widget

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
-- Properties
--------------------------------------------------------------------------------

active ::
     Group -- ^
  -> StateVar Bool
active =
  wrapped Widget.active

align ::
     Group -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Widget.align

box ::
     Group -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Widget.box

changed ::
     Group -- ^
  -> StateVar Bool
changed =
  wrapped Widget.changed

color ::
     Group -- ^
  -> StateVar Fltk.Color
color =
  wrapped Widget.color

damage ::
     Group -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Widget.damage

deimage ::
     Group -- ^
  -> StateVar (Maybe Image)
deimage =
  wrapped Widget.deimage

flags ::
     Group -- ^
  -> StateVar [Fltk.WidgetFlag]
flags =
  wrapped Widget.flags

image ::
     Group -- ^
  -> StateVar (Maybe Image)
image =
  wrapped Widget.image

label ::
     Group -- ^
  -> StateVar Text
label =
  wrapped Widget.label

labelColor ::
     Group -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Widget.labelColor

labelFont ::
     Group -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Widget.labelFont

labelSize ::
     Group -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Widget.labelSize

labelType ::
     Group -- ^
  -> StateVar Fltk.Labeltype
labelType =
  wrapped Widget.labelType

output ::
     Group -- ^
  -> StateVar Bool
output =
  wrapped Widget.output

parent ::
     Group -- ^
  -> StateVar (Maybe Group)
parent =
  wrapped Widget.parent

{-
https://github.com/deech/fltkhs/issues/119

resizable ::
     Group -- ^
  -> StateVar (Maybe Widget)
resizable =
  wrapped Group.resizable
-}

selectionColor ::
     Group -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Widget.selectionColor

tooltip ::
     Group -- ^
  -> StateVar Text
tooltip =
  wrapped Widget.tooltip

type_ ::
     Group -- ^
  -> StateVar Word8
type_ =
  wrapped Widget.type_

visible ::
     Group -- ^
  -> StateVar Bool
visible =
  wrapped Widget.visible

visibleFocus ::
     Group -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Widget.visibleFocus

when ::
     Group -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Widget.when


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

activeR ::
     Group -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

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

children ::
     Group -- ^
  -> IO Int
children =
  wrapped Fltk.children

clear ::
     Group -- ^
  -> IO ()
clear =
  wrapped Fltk.clear

clipChildren ::
     Group -- ^
  -> IO Bool
clipChildren =
  wrapped Fltk.clipChildren

contains ::
     Group -- ^
  -> Widget -- ^
  -> IO Bool
contains group widget =
  wrapped Fltk.contains group (unWidget widget)

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

drawBackdrop ::
     Group -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Group -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Group -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawChild ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
drawChild group widget =
  wrapped Fltk.drawChild group (unWidget widget)

drawChildren ::
     Group -- ^
  -> IO ()
drawChildren =
  wrapped Fltk.drawChildren

drawFocus ::
     Group -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle) -- ^
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Group -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments) -- ^
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

drawOutsideLabel ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
drawOutsideLabel group widget =
  wrapped Fltk.drawOutsideLabel group (unWidget widget)

end ::
     Group -- ^
  -> IO ()
end =
  wrapped Fltk.end

find ::
     Group -- ^
  -> Widget -- ^
  -> IO Fltk.AtIndex
find group widget =
  wrapped Fltk.find group (unWidget widget)

focus ::
     Group -- ^
  -> Widget -- ^
  -> IO ()
focus group widget =
  wrapped Fltk.focus group (unWidget widget)

getArray ::
     Group -- ^
  -> IO [Widget]
getArray =
  coerce (wrapped Fltk.getArray)

getCallback ::
     Group -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getChild ::
     Group -- ^
  -> Fltk.AtIndex -- ^
  -> IO (Maybe Widget)
getChild =
  coerce (wrapped Fltk.getChild)

getH ::
     Group -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getRectangle ::
     Group -- ^
  -> IO Fltk.Rectangle
getRectangle =
  wrapped Fltk.getRectangle

getTopWindow ::
     Group -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Group -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getVisibleR ::
     Group -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Group -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWindow ::
     Group -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Group -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Group -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Group -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Group -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

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

inside ::
     Group -- ^
  -> Widget -- ^
  -> IO Bool
inside group widget =
  wrapped Fltk.inside group (unWidget widget)

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

resize ::
     Group -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
resize =
  wrapped Fltk.resize

setCallback ::
     Group -- ^
  -> (Group -> IO ()) -- ^
  -> IO ()
setCallback group callback =
  wrapped Fltk.setCallback group (coerce callback)

setClipChildren ::
     Group -- ^
  -> Bool -- ^
  -> IO ()
setClipChildren =
  wrapped Fltk.setClipChildren

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

takesevents ::
     Group -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents

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
