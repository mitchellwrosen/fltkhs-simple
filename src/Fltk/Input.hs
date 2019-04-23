module Fltk.Input
  ( -- * Input
    Input
  , new
    -- * API
    -- ** Properties
  , active
  , align
  , box
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
  , changed
  , clearChanged
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
  , contains
  , copy
  , copyCuts
  , copyTooltip
  , cut
  , cutFromCursor
  , cutRange
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , drawText
  , flags
  , getCallback
  , getCursorColor
  , getDeimage
  , getH
  , getImage
  , getInputType
  , getLabeltype
  , getMark
  , getMaximumSize
  , getOutput
  , getParent
  , getPosition
  , getReadonly
  , getRectangle
  , getShortcut
  , getSize
  , getTabNav
  , getTextcolor
  , getTextfont
  , getTextsize
  , getTooltip
  , getTopWindow
  , getTopWindowOffset
  , getValue
  , getVisibleR
  , getW
  , getWindow
  , getWrap
  , getX
  , getY
  , handle
  , hasCallback
  , index
  , insert
  , insertWithLength
  , inside
  , measureLabel
  , redraw
  , redrawLabel
  , replace
  , resize
  , setAlign
  , setCallback
  , setChanged
  , setColorWithBgSel
  , setCursorColor
  , setDamageInside
  , setDeimage
  , setFlag
  , setImage
  , setInputType
  , setLabeltype
  , setMark
  , setMaximumSize
  , setOutput
  , setParent
  , setPosition
  , setReadonly
  , setShortcut
  , setSize
  , setTabNav
  , setTextcolor
  , setTextfont
  , setTextsize
  , setValue
  , setWrap
  , staticValue
  , takeFocus
  , takesevents
  , undo
  ) where

import Fltk.Types.Internal (Group(..), Image(..), Input(..), IsGroup(..),
                            IsImage(..), IsWidget(..), Window(..))

import qualified Fltk.Internal.Widget as Widget

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
-- Properties
--------------------------------------------------------------------------------

active ::
     Input -- ^
  -> StateVar Bool
active =
  wrapped Widget.active

align ::
     Input -- ^
  -> StateVar Fltk.Alignments
align =
  wrapped Widget.align

box ::
     Input -- ^
  -> StateVar Fltk.Boxtype
box =
  wrapped Widget.box

color ::
     Input -- ^
  -> StateVar Fltk.Color
color =
  wrapped Widget.color

damage ::
     Input -- ^
  -> StateVar [Fltk.Damage]
damage =
  wrapped Widget.damage

label ::
     Input -- ^
  -> StateVar Text
label =
  wrapped Widget.label

labelColor ::
     Input -- ^
  -> StateVar Fltk.Color
labelColor =
  wrapped Widget.labelColor

labelFont ::
     Input -- ^
  -> StateVar Fltk.Font
labelFont =
  wrapped Widget.labelFont

labelSize ::
     Input -- ^
  -> StateVar Fltk.FontSize
labelSize =
  wrapped Widget.labelSize

selectionColor ::
     Input -- ^
  -> StateVar Fltk.Color
selectionColor =
  wrapped Widget.selectionColor

tooltip ::
     Input -- ^
  -> StateVar Text
tooltip =
  wrapped Widget.tooltip

type_ ::
     Input -- ^
  -> StateVar Word8
type_ =
  wrapped Widget.type_

visible ::
     Input -- ^
  -> StateVar Bool
visible =
  wrapped Widget.visible

visibleFocus ::
     Input -- ^
  -> StateVar Bool
visibleFocus =
  wrapped Widget.visibleFocus

when ::
     Input -- ^
  -> StateVar [Fltk.When]
when =
  wrapped Widget.when


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------


activeR ::
     Input -- ^
  -> IO Bool
activeR =
  wrapped Fltk.activeR

changed ::
     Input -- ^
  -> IO Bool
changed =
  wrapped Fltk.changed

clearChanged ::
     Input -- ^
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Input -- ^
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Input -- ^
  -> [Fltk.Damage]
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Input -- ^
  -> Fltk.WidgetFlag
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Input -- ^
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

contains ::
     IsWidget widget
  => Input -- ^
  -> widget -- ^
  -> IO Bool
contains input widget =
  asWidget widget (wrapped Fltk.contains input)

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

drawBackdrop ::
     Input -- ^
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Input -- ^
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Input -- ^
  -> Fltk.Boxtype -- ^
  -> Fltk.Color -- ^
  -> Maybe Fltk.Rectangle -- ^
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawFocus ::
     Input -- ^
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle)
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Input -- ^
  -> Maybe (Fltk.Rectangle, Fltk.Alignments)
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

flags ::
     Input -- ^
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

getCallback ::
     Input -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getCursorColor ::
     Input -- ^
  -> IO Fltk.Color
getCursorColor =
  wrapped Fltk.getCursorColor

getDeimage ::
     Input -- ^
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Input -- ^
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

getImage ::
     Input -- ^
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getInputType ::
     Input -- ^
  -> IO Fltk.FlInputType
getInputType =
  wrapped Fltk.getInputType

getLabeltype ::
     Input -- ^
  -> IO Fltk.Labeltype
getLabeltype =
  wrapped Fltk.getLabeltype

getMark ::
     Input -- ^
  -> IO Int
getMark =
  wrapped Fltk.getMark

getMaximumSize ::
     Input -- ^
  -> IO Int
getMaximumSize =
  wrapped Fltk.getMaximumSize

getOutput ::
     Input -- ^
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

getParent ::
     Input -- ^
  -> IO (Maybe Group)
getParent =
  coerce (wrapped Fltk.getParent)

getPosition ::
     Input -- ^
  -> IO Int
getPosition =
  wrapped Fltk.getPosition

getReadonly ::
     Input -- ^
  -> IO Bool
getReadonly =
  wrapped Fltk.getReadonly

getRectangle ::
     Input -- ^
  -> IO Fltk.Rectangle
getRectangle =
  wrapped Fltk.getRectangle

getShortcut ::
     Input -- ^
  -> IO (Maybe Fltk.ShortcutKeySequence)
getShortcut =
  wrapped Fltk.getShortcut

getSize ::
     Input -- ^
  -> IO Int
getSize =
  wrapped Fltk.getSize

getTabNav ::
     Input -- ^
  -> IO Bool
getTabNav =
  wrapped Fltk.getTabNav

getTextcolor ::
     Input -- ^
  -> IO Fltk.Color
getTextcolor =
  wrapped Fltk.getTextcolor

getTextfont ::
     Input -- ^
  -> IO Fltk.Font
getTextfont =
  wrapped Fltk.getTextfont

getTextsize ::
     Input -- ^
  -> IO Fltk.FontSize
getTextsize =
  wrapped Fltk.getTextsize

getTooltip ::
     Input -- ^
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

getTopWindow ::
     Input -- ^
  -> IO (Maybe Window)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Input -- ^
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getValue ::
     Input -- ^
  -> IO Text
getValue =
  wrapped Fltk.getValue

getVisibleR ::
     Input -- ^
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Input -- ^
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWindow ::
     Input -- ^
  -> IO (Maybe Window)
getWindow =
  coerce (wrapped Fltk.getWindow)

getWrap ::
     Input -- ^
  -> IO Bool
getWrap =
  wrapped Fltk.getWrap

getX ::
     Input -- ^
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

getY ::
     Input -- ^
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

handle ::
     Input -- ^
  -> Fltk.Event -- ^
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Input -- ^
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

index ::
     Input -- ^
  -> Fltk.AtIndex -- ^
  -> IO Char
index =
  wrapped Fltk.index

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

inside ::
     IsWidget widget
  => Input -- ^
  -> widget -- ^
  -> IO Bool
inside input widget =
  asWidget widget (wrapped Fltk.inside input)

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

setAlign ::
     Input -- ^
  -> Fltk.Alignments
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

setCallback ::
     Input -- ^
  -> (Input -> IO ()) -- ^
  -> IO ()
setCallback input callback =
  wrapped Fltk.setCallback input (coerce callback)

setChanged ::
     Input  -- ^
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setColorWithBgSel ::
     Input -- ^
  -> Fltk.Color -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

setCursorColor ::
     Input -- ^
  -> Fltk.Color -- ^
  -> IO ()
setCursorColor =
  wrapped Fltk.setCursorColor

setDamageInside ::
     Input -- ^
  -> [Fltk.Damage] -- ^
  -> Fltk.Rectangle -- ^
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

setDeimage ::
     IsImage image
  => Input -- ^
  -> Maybe image -- ^
  -> IO ()
setDeimage input = \case
  Nothing ->
    wrapped Fltk.setDeimage input (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setDeimage input (Just ref))

setFlag ::
     Input -- ^
  -> Fltk.WidgetFlag -- ^
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

setImage ::
     IsImage image
  => Input -- ^
  -> Maybe image -- ^
  -> IO ()
setImage input = \case
  Nothing ->
    wrapped Fltk.setImage input (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> wrapped Fltk.setImage input (Just ref))

setInputType ::
     Input -- ^
  -> Fltk.FlInputType -- ^
  -> IO ()
setInputType =
  wrapped Fltk.setInputType

setLabeltype ::
     Input -- ^
  -> Fltk.Labeltype -- ^
  -> Fltk.ResolveImageLabelConflict -- ^
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

setMark ::
     Input -- ^
  -> Int -- ^
  -> IO (Either Fltk.NoChange ())
setMark =
  wrapped Fltk.setMark

setMaximumSize ::
     Input -- ^
  -> Int -- ^
  -> IO ()
setMaximumSize =
  wrapped Fltk.setMaximumSize

setOutput ::
     Input -- ^
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

setParent ::
     IsGroup group
  => Input -- ^
  -> Maybe group -- ^
  -> IO ()
setParent input = \case
  Nothing ->
    wrapped Fltk.setParent input (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroup group (\ref -> wrapped Fltk.setParent input (Just ref))

setPosition ::
     Input -- ^
  -> Int -- ^
  -> Maybe Int -- ^
  -> IO (Either Fltk.NoChange ())
setPosition =
  wrapped Fltk.setPosition

setReadonly ::
     Input -- ^
  -> Bool -- ^
  -> IO ()
setReadonly =
  wrapped Fltk.setReadonly

setShortcut ::
     Input -- ^
  -> Fltk.ShortcutKeySequence -- ^
  -> IO ()
setShortcut =
  wrapped Fltk.setShortcut

setSize ::
     Input -- ^
  -> Fltk.Size -- ^
  -> IO ()
setSize =
  wrapped Fltk.setSize

setTabNav ::
     Input -- ^
  -> Bool -- ^
  -> IO ()
setTabNav =
  wrapped Fltk.setTabNav

setTextcolor ::
     Input -- ^
  -> Fltk.Color -- ^
  -> IO ()
setTextcolor =
  wrapped Fltk.setTextcolor

setTextfont ::
     Input -- ^
  -> Fltk.Font -- ^
  -> IO ()
setTextfont =
  wrapped Fltk.setTextfont

setTextsize ::
     Input -- ^
  -> Fltk.FontSize -- ^
  -> IO ()
setTextsize =
  wrapped Fltk.setTextsize

setValue ::
     Input -- ^
  -> Text -- ^
  -> IO (Either Fltk.NoChange ())
setValue =
  wrapped Fltk.setValue

setWrap ::
     Input -- ^
  -> Bool -- ^
  -> IO ()
setWrap =
  wrapped Fltk.setWrap

staticValue ::
     Input -- ^
  -> Text -- ^
  -> IO (Either Fltk.NoChange ())
staticValue =
  wrapped Fltk.staticValue

takeFocus ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Input -- ^
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents

undo ::
     Input -- ^
  -> IO (Either Fltk.NoChange ())
undo =
  wrapped Fltk.undo
