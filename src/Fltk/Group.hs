module Fltk.Group
  ( -- * Group
    Group
  , IsGroup
  , new
    -- * API
    -- ** Properties
  , active
  , align
  , box
  , changed
  , color
  , damage
  , label
  , labelColor
  , labelFont
  , labelSize
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
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
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
  , flags
  , focus
  , getArray
  , getCallback
  , getChild
  , getDeimage
  , getH
  , getImage
  , getLabeltype
  , getOutput
  , getParent
  , getRectangle
  , getResizable
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
  , setDeimage
  , setFlag
  , setImage
  , setLabeltype
  , setNotResizable
  , setOutput
  , setParent
  , setResizable
  , takeFocus
  , takesevents
  , updateChild
  , within
  ) where

import Fltk.Types.Internal (Group(..), Image(..), IsGroup(..), IsImage(..),
                            IsWidget(..), Widget(..), Window(..))

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
add group widget =
  asWidget widget (wrapped Fltk.add group)

addResizable ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
addResizable group widget =
  asWidget widget (wrapped Fltk.addResizable group)

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

clearDamage ::
     Group -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Group -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Group -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Group -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clipChildren ::
     Group -- ^
  -> IO Bool
clipChildren =
  wrapped Fltk.clipChildren

contains ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Bool
contains group widget =
  asWidget widget (wrapped Fltk.contains group)

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
drawChild group widget =
  asWidget widget (wrapped Fltk.drawChild group)

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
drawOutsideLabel group widget =
  asWidget widget (wrapped Fltk.drawOutsideLabel group)

end ::
     Group -- ^
  -> IO ()
end =
  wrapped Fltk.end

find ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Fltk.AtIndex
find group widget =
  asWidget widget (wrapped Fltk.find group)

flags ::
     Group -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

focus ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
focus group widget =
  asWidget widget (wrapped Fltk.focus group)

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

getDeimage ::
     Group -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Group -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Group -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabeltype ::
     Group -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getOutput ::
     Group -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Group -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Group -- ^
  -> IO Fltk.Rectangle
getRectangle =
  wrapped Fltk.getRectangle

getResizable ::
     Group -- ^
  -> IO (Maybe Widget)
getResizable =
  coerce (wrapped Fltk.getResizable)

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> Fltk.AtIndex -- ^
  -> IO ()
insert group widget index =
  asWidget widget (\ref -> wrapped Fltk.insert group ref index)

-- insertBefore :: (Parent a WidgetBase) => Group -> Ref a -> Ref b -> IO ()
-- insertBefore=
--   wrapped Fltk.insertBefore

inside ::
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO Bool
inside group widget =
  asWidget widget (wrapped Fltk.inside group)

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
removeWidget group widget =
  asWidget widget (wrapped Fltk.removeWidget group)

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

setDeimage ::
     IsImage image
  => Group -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage group = \case
  Nothing ->
    wrapped Fltk.setDeimage group (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage group (Just ref))

setFlag ::
     Group -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Group -- ^
  -> Maybe image -- ^
  -> IO ()
setImage group = \case
  Nothing ->
    wrapped Fltk.setImage group (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage group (Just ref))

setLabeltype ::
     Group -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setNotResizable ::
     Group -- ^
  -> IO ()
setNotResizable =
  wrapped Fltk.setNotResizable

setOutput ::
     Group -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Group -- ^
  -> Maybe group -- ^
  -> IO ()
setParent group = \case
  Nothing ->
    wrapped Fltk.setParent group (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just parent ->
    asGroup parent (\ref -> wrapped Fltk.setParent group (Just ref))

setResizable ::
     IsWidget widget
  => Group -- ^
  -> Maybe widget -- ^
  -> IO ()
setResizable group = \case
  Nothing ->
    wrapped Fltk.setResizable group (Nothing @(Fltk.Ref Fltk.WidgetBase))
  Just widget ->
    asWidget widget (\ref -> wrapped Fltk.setResizable group (Just ref))

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
     IsWidget widget
  => Group -- ^
  -> widget -- ^
  -> IO ()
updateChild group widget =
  asWidget widget (wrapped Fltk.updateChild group)

within ::
     Group -- ^
  -> IO a -- ^
  -> IO a
within =
  wrapped Fltk.within
