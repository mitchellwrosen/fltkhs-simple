module Fltk.Input
  ( -- * Input
    Input
  , new
    -- * API
  , activate
  , active
  , activeR
  , changed
  , clearActive
  , clearChanged
  , clearDamage
  , clearDamageThenSet
  , clearFlag
  , clearOutput
  , clearVisible
  , clearVisibleFocus
  , contains
  , copy
  , copyCuts
  , copyTooltip
  , cut
  , cutFromCursor
  , cutRange
  , deactivate
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , drawText
  , flags
  , getAlign
  , getBox
  , getCallback
  , getColor
  , getCursorColor
  , getDamage
  , getDeimage
  , getH
  , getImage
  , getInputType
  , getLabel
  , getLabelcolor
  , getLabelfont
  , getLabelsize
  , getLabeltype
  , getMark
  , getMaximumSize
  , getOutput
  , getParent
  , getPosition
  , getReadonly
  , getRectangle
  , getSelectionColor
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
  , getVisible
  , getVisibleFocus
  , getVisibleR
  , getW
  , getWhen
  , getWindow
  , getWrap
  , getX
  , getY
  , handle
  , hasCallback
  , hide
  , index
  , insert
  , insertWithLength
  , inside
  , measureLabel
  , modifyVisibleFocus
  , redraw
  , redrawLabel
  , replace
  , resize
  , setActive
  , setAlign
  , setBox
  , setCallback
  , setChanged
  , setColor
  , setColorWithBgSel
  , setCursorColor
  , setDamage
  , setDamageInside
  , setDeimage
  , setFlag
  , setImage
  , setInputType
  , setLabel
  , setLabelcolor
  , setLabelfont
  , setLabelsize
  , setLabeltype
  , setMark
  , setMaximumSize
  , setOutput
  , setParent
  , setPosition
  , setReadonly
  , setSelectionColor
  , setShortcut
  , setSize
  , setTabNav
  , setTextcolor
  , setTextfont
  , setTextsize
  , setTooltip
  , setValue
  , setVisible
  , setVisibleFocus
  , setWhen
  , setWrap
  , showWidget
  , staticValue
  , takeFocus
  , takesevents
  , undo
  ) where

import Fltk.Types.Internal (Group(..), Image(..), Input(..), IsGroup(..),
                            IsImage(..), IsWidget(..), Window(..))

import Data.Coerce (coerce)
import Data.Text   (Text)
import Foreign.Ptr (FunPtr)

import qualified Graphics.UI.FLTK.LowLevel.Base.Input      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


wrapped ::
     (Fltk.Ref Fltk.InputBase -> a)
  -> Input
  -> a
wrapped =
  coerce

new ::
     Fltk.Rectangle -- ^
  -> Text -- ^
  -> Maybe Fltk.FlInputType
  -> IO Input
new bounds label typ =
  Input . Fltk.safeCast <$> Fltk.inputNew bounds (Just label) typ

activate ::
     Input -- ^
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Input -- ^
  -> IO Bool
active =
  wrapped Fltk.active

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

clearActive ::
     Input -- ^
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

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

clearVisible ::
     Input -- ^
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Input -- ^
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

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

deactivate ::
     Input -- ^
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

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

getAlign ::
     Input -- ^
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getBox ::
     Input -- ^
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Input -- ^
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getColor ::
     Input -- ^
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getCursorColor ::
     Input -- ^
  -> IO Fltk.Color
getCursorColor =
  wrapped Fltk.getCursorColor

getDamage ::
     Input -- ^
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

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

getLabel ::
     Input -- ^
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Input -- ^
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Input -- ^
  -> IO Fltk.Font
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Input -- ^
  -> IO Fltk.FontSize
getLabelsize =
  wrapped Fltk.getLabelsize

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

getSelectionColor ::
     Input -- ^
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

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

getVisible ::
     Input -- ^
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Input -- ^
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

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

getWhen ::
     Input -- ^
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

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

hide ::
     Input -- ^
  -> IO ()
hide =
  wrapped Fltk.hide

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

modifyVisibleFocus ::
     Input -- ^
  -> Bool -- ^
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

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

setActive ::
     Input -- ^
  -> IO ()
setActive =
  wrapped Fltk.setActive

setAlign ::
     Input -- ^
  -> Fltk.Alignments
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

setBox ::
     Input -- ^
  -> Fltk.Boxtype -- ^
  -> IO ()
setBox =
  wrapped Fltk.setBox

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

setColor ::
     Input -- ^
  -> Fltk.Color -- ^
  -> IO ()
setColor =
  wrapped Fltk.setColor

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

setDamage ::
     Input -- ^
  -> [Fltk.Damage] -- ^
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

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

setLabel ::
     Input -- ^
  -> Text -- ^
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Input -- ^
  -> Fltk.Color -- ^
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Input -- ^
  -> Fltk.Font -- ^
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Input -- ^
  -> Fltk.FontSize -- ^
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

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

setSelectionColor ::
     Input -- ^
  -> Fltk.Color -- ^
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

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

setTooltip ::
     Input -- ^
  -> Text -- ^
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

setValue ::
     Input -- ^
  -> Text -- ^
  -> IO (Either Fltk.NoChange ())
setValue =
  wrapped Fltk.setValue

setVisible ::
     Input -- ^
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Input -- ^
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Input -- ^
  -> [Fltk.When] -- ^
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

setWrap ::
     Input -- ^
  -> Bool -- ^
  -> IO ()
setWrap =
  wrapped Fltk.setWrap

showWidget ::
     Input -- ^
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

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
