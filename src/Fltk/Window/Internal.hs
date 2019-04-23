module Fltk.Window.Internal where

import Fltk.GroupBase.Internal  (GroupBase(..), IsGroupBase(..))
import Fltk.Image.Internal      (Image(..), IsImage(..))
import Fltk.Widget.Internal     (Widget(..))
import Fltk.WidgetBase.Internal (IsWidgetBase(..), WidgetBase(..))
import Fltk.WindowBase.Internal (IsWindowBase(..), WindowBase(..))

import Data.Coerce                          (coerce)
import Data.Text                            (Text)
import Data.Word                            (Word8)
import Foreign.Ptr                          (FunPtr)
import Graphics.UI.FLTK.LowLevel.Base.Group ()
import Graphics.UI.FLTK.LowLevel.Window     ()

import qualified Graphics.UI.FLTK.LowLevel.Base.Widget     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Base.Window     as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types        as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy       as Fltk


newtype Window
  = Window { unWindow :: Fltk.Ref Fltk.Window }

instance IsGroupBase Window where
  asGroupBase _ _ = undefined

instance IsWidgetBase Window where
  asWidgetBase _ _ = undefined

instance IsWindowBase Window where
  asWindowBase _ _ = undefined

wrapped ::
     (Fltk.Ref Fltk.Window -> a)
  -> Window
  -> a
wrapped =
  coerce

new ::
     Fltk.Size
  -> Maybe Fltk.Position
  -> Text
  -> IO Window
new size pos title =
  coerce (Fltk.windowNew size pos (Just title))


activate ::
     Window
  -> IO ()
activate =
  wrapped Fltk.activate

active ::
     Window
  -> IO Bool
active =
  wrapped Fltk.active

activeR ::
     Window
  -> IO Bool
activeR =
  wrapped Fltk.activeR

add ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
add window widget =
  asWidgetBase widget (Fltk.add (unWindow window))

addResizable ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
addResizable window widget =
  asWidgetBase widget (Fltk.addResizable (unWindow window))

begin ::
     Window
  -> IO ()
begin =
  wrapped Fltk.begin

changed ::
     Window
  -> IO Bool
changed =
  wrapped Fltk.changed

children ::
     Window
  -> IO Int
children =
  wrapped Fltk.children

clear ::
     Window
  -> IO ()
clear =
  wrapped Fltk.clear

clearActive ::
     Window
  -> IO ()
clearActive =
  wrapped Fltk.clearActive

-- clearBorder :: Ref WindowBase -> IO ()

clearChanged ::
     Window
  -> IO ()
clearChanged =
  wrapped Fltk.clearChanged

clearDamage ::
     Window
  -> IO ()
clearDamage =
  wrapped Fltk.clearDamage

clearDamageThenSet ::
     Window
  -> [Fltk.Damage]
  -> IO ()
clearDamageThenSet =
  wrapped Fltk.clearDamageThenSet

clearFlag ::
     Window
  -> Fltk.WidgetFlag
  -> IO ()
clearFlag =
  wrapped Fltk.clearFlag

clearOutput ::
     Window
  -> IO ()
clearOutput =
  wrapped Fltk.clearOutput

clearVisible ::
     Window
  -> IO ()
clearVisible =
  wrapped Fltk.clearVisible

clearVisibleFocus ::
     Window
  -> IO ()
clearVisibleFocus =
  wrapped Fltk.clearVisibleFocus

clipChildren ::
     Window
  -> IO Bool
clipChildren =
  wrapped Fltk.clipChildren

contains ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO Bool
contains box widget =
  asWidgetBase widget (Fltk.contains (unWindow box))

-- copyLabel :: Ref WindowBase -> Text -> IO ()

copyTooltip ::
     Window
  -> Text
  -> IO ()
copyTooltip =
  wrapped Fltk.copyTooltip

ddfdesignKludge ::
     Window
  -> IO (Maybe WidgetBase)
ddfdesignKludge =
  coerce (wrapped Fltk.ddfdesignKludge)

deactivate ::
     Window
  -> IO ()
deactivate =
  wrapped Fltk.deactivate

destroy ::
     Window
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     Window
  -> IO ()
doCallback =
  wrapped Fltk.doCallback

-- draw ::
--      Window
--   -> IO ()
-- draw =
--   wrapped Fltk.draw

drawBackdrop ::
     Window
  -> IO ()
drawBackdrop =
  wrapped Fltk.drawBackdrop

drawBox ::
     Window
  -> IO ()
drawBox =
  wrapped Fltk.drawBox

drawBoxWithBoxtype ::
     Window
  -> Fltk.Boxtype
  -> Fltk.Color
  -> Maybe Fltk.Rectangle
  -> IO ()
drawBoxWithBoxtype =
  wrapped Fltk.drawBoxWithBoxtype

drawChild ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
drawChild window widget =
  asWidgetBase widget (Fltk.drawChild (unWindow window))

drawChildren ::
     Window
  -> IO ()
drawChildren =
  wrapped Fltk.drawChildren

drawFocus ::
     Window
  -> Maybe (Fltk.Boxtype, Fltk.Rectangle)
  -> IO ()
drawFocus =
  wrapped Fltk.drawFocus

drawLabel ::
     Window
  -> Maybe (Fltk.Rectangle, Fltk.Alignments)
  -> IO ()
drawLabel =
  wrapped Fltk.drawLabel

drawOutsideLabel ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
drawOutsideLabel window widget =
  asWidgetBase widget (Fltk.drawOutsideLabel (unWindow window))

end ::
     Window
  -> IO ()
end =
  wrapped Fltk.end

find ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO Fltk.AtIndex
find window widget =
  asWidgetBase widget (Fltk.find (unWindow window))

flags ::
     Window
  -> IO [Fltk.WidgetFlag]
flags =
  wrapped Fltk.flags

-- flush :: Ref WindowBase -> IO ()

focus ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
focus window widget =
  asWidgetBase widget (Fltk.focus (unWindow window))

-- freePosition :: Ref WindowBase -> IO ()

-- fullscreenOff :: Ref WindowBase -> Maybe Rectangle -> IO ()

getAlign ::
     Window
  -> IO Fltk.Alignments
getAlign =
  wrapped Fltk.getAlign

getArray ::
     Window
  -> IO [WidgetBase]
getArray =
  coerce (wrapped Fltk.getArray)

-- getBorder :: Ref WindowBase -> IO (Bool)

getBox ::
     Window
  -> IO Fltk.Boxtype
getBox =
  wrapped Fltk.getBox

getCallback ::
     Window
  -> IO (FunPtr Fltk.CallbackWithUserDataPrim)
getCallback =
  wrapped Fltk.getCallback

getChild ::
     Window
  -> Fltk.AtIndex
  -> IO (Maybe WidgetBase)
getChild =
  coerce (wrapped Fltk.getChild)

getColor ::
     Window
  -> IO Fltk.Color
getColor =
  wrapped Fltk.getColor

getDamage ::
     Window
  -> IO [Fltk.Damage]
getDamage =
  wrapped Fltk.getDamage

-- getDecoratedH :: Ref WindowBase -> IO (Int)

-- getDecoratedW :: Ref WindowBase -> IO (Int)

getDeimage ::
     Window
  -> IO (Maybe Image)
getDeimage =
  coerce (wrapped Fltk.getDeimage)

getH ::
     Window
  -> IO Fltk.Height
getH =
  wrapped Fltk.getH

-- getIcon :: Ref WindowBase -> IO (Maybe (Ref Image))

-- getIconlabel :: Ref WindowBase -> IO Text

getImage ::
     Window
  -> IO (Maybe Image)
getImage =
  coerce (wrapped Fltk.getImage)

getLabel ::
     Window
  -> IO Text
getLabel =
  wrapped Fltk.getLabel

getLabelcolor ::
     Window
  -> IO Fltk.Color
getLabelcolor =
  wrapped Fltk.getLabelcolor

getLabelfont ::
     Window
  -> IO (Fltk.Font)
getLabelfont =
  wrapped Fltk.getLabelfont

getLabelsize ::
     Window
  -> IO (Fltk.FontSize)
getLabelsize =
  wrapped Fltk.getLabelsize

getLabeltype ::
     Window
  -> IO (Fltk.Labeltype)
getLabeltype =
  wrapped Fltk.getLabeltype

-- getMenuWindow :: Ref WindowBase -> IO (Bool)

-- getModal :: Ref WindowBase -> IO (Bool)

getOutput ::
     Window
  -> IO Int
getOutput =
  wrapped Fltk.getOutput

-- getOverride :: Ref WindowBase -> IO (Bool)

getParent ::
     Window
  -> IO (Maybe GroupBase)
getParent =
  coerce (wrapped Fltk.getParent)

getRectangle ::
     Window
  -> IO Fltk.Rectangle
getRectangle =
 wrapped Fltk.getRectangle

getResizable ::
     Window
  -> IO (Maybe Widget)
getResizable =
  coerce (wrapped Fltk.getResizable)

getSelectionColor ::
     Window
  -> IO Fltk.Color
getSelectionColor =
  wrapped Fltk.getSelectionColor

getTooltip ::
     Window
  -> IO Text
getTooltip =
  wrapped Fltk.getTooltip

-- getTooltipWindow :: Ref WindowBase -> IO (Bool)

getTopWindow ::
     Window
  -> IO (Maybe WindowBase)
getTopWindow =
  coerce (wrapped Fltk.getTopWindow)

getTopWindowOffset ::
     Window
  -> IO Fltk.Position
getTopWindowOffset =
  wrapped Fltk.getTopWindowOffset

getType_ ::
     Window
  -> IO Fltk.WindowType
getType_ =
  wrapped Fltk.getType_

getVisible ::
     Window
  -> IO Bool
getVisible =
  wrapped Fltk.getVisible

getVisibleFocus ::
     Window
  -> IO Bool
getVisibleFocus =
  wrapped Fltk.getVisibleFocus

getVisibleR ::
     Window
  -> IO Bool
getVisibleR =
  wrapped Fltk.getVisibleR

getW ::
     Window
  -> IO Fltk.Width
getW =
  wrapped Fltk.getW

getWhen ::
     Window
  -> IO [Fltk.When]
getWhen =
  wrapped Fltk.getWhen

getWindow ::
     Window
  -> IO (Maybe WindowBase)
getWindow =
  coerce (wrapped Fltk.getWindow)

getX ::
     Window
  -> IO Fltk.X
getX =
  wrapped Fltk.getX

-- getXRoot :: Ref WindowBase -> IO (Int)

-- getXclass :: Ref WindowBase -> IO Text

getY ::
     Window
  -> IO Fltk.Y
getY =
  wrapped Fltk.getY

-- getYRoot :: Ref WindowBase -> IO (Int)

handle ::
     Window
  -> Fltk.Event
  -> IO (Either Fltk.UnknownEvent ())
handle =
  wrapped Fltk.handle

hasCallback ::
     Window
  -> IO Bool
hasCallback =
  wrapped Fltk.hasCallback

hide ::
     Window
  -> IO ()
hide =
  wrapped Fltk.hide

-- hotSpot :: Ref WindowBase -> PositionSpec -> Maybe Bool -> IO ()

-- iconize :: Ref WindowBase -> IO ()

initSizes ::
     Window
  -> IO ()
initSizes =
  wrapped Fltk.initSizes

insert ::
     IsWidgetBase widget
  => Window
  -> widget
  -> Fltk.AtIndex
  -> IO ()
insert window widget index =
  asWidgetBase widget (\ref -> Fltk.insert (unWindow window) ref index)

-- insertBefore ::
--      IsWidgetBase widget
--   => Window GroupBase
--   -> widget
--   -> Ref b
--   -> IO ()

inside ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO Bool
inside box widget =
  asWidgetBase widget (Fltk.inside (unWindow box))

-- makeCurrent :: Ref WindowBase -> IO ()

-- makeFullscreen :: Ref WindowBase -> IO ()

measureLabel ::
     Window
  -> Maybe Fltk.Width
  -> IO Fltk.Size
measureLabel =
  wrapped Fltk.measureLabel

modifyVisibleFocus ::
     Window
  -> Bool
  -> IO ()
modifyVisibleFocus =
  wrapped Fltk.modifyVisibleFocus

-- nonModal :: Ref WindowBase -> IO (Bool)

redraw ::
     Window
  -> IO ()
redraw =
  wrapped Fltk.redraw

redrawLabel ::
     Window
  -> IO ()
redrawLabel =
  wrapped Fltk.redrawLabel

removeIndex ::
     Window
  -> Fltk.AtIndex
  -> IO ()
removeIndex =
  wrapped Fltk.removeIndex

removeWidget ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
removeWidget window widget =
  asWidgetBase widget (Fltk.removeWidget (unWindow window))

resize ::
     Window
  -> Fltk.Rectangle
  -> IO ()
resize =
  wrapped Fltk.resize

setActive ::
     Window
  -> IO ()
setActive =
  wrapped Fltk.setActive

setAlign ::
     Window
  -> Fltk.Alignments
  -> IO ()
setAlign =
  wrapped Fltk.setAlign

-- setBorder :: Ref WindowBase -> Bool -> IO ()

setBox ::
     Window
  -> Fltk.Boxtype
  -> IO ()
setBox =
  wrapped Fltk.setBox

setCallback ::
     Window
  -> (Window -> IO ())
  -> IO ()
setCallback box callback =
  Fltk.setCallback (unWindow box) (coerce callback)

setChanged ::
     Window
  -> IO ()
setChanged =
  wrapped Fltk.setChanged

setClipChildren ::
     Window
  -> Bool
  -> IO ()
setClipChildren =
  wrapped Fltk.setClipChildren

setColor ::
     Window
  -> Fltk.Color
  -> IO ()
setColor =
  wrapped Fltk.setColor

setColorWithBgSel ::
     Window
  -> Fltk.Color
  -> Fltk.Color
  -> IO ()
setColorWithBgSel =
  wrapped Fltk.setColorWithBgSel

-- setCursor :: Ref WindowBase -> Cursor -> IO ()

-- setCursorWithFgBg :: Ref WindowBase -> Cursor -> (Maybe Color, Maybe Color) -> IO ()

setDamage ::
     Window
  -> [Fltk.Damage]
  -> IO ()
setDamage =
  wrapped Fltk.setDamage

setDamageInside ::
     Window
  -> [Fltk.Damage]
  -> Fltk.Rectangle
  -> IO ()
setDamageInside =
  wrapped Fltk.setDamageInside

-- setDefaultCursor :: Ref WindowBase -> CursorType -> IO ()

-- setDefaultCursorWithFgBg :: Ref WindowBase -> CursorType -> (Maybe Color, Maybe Color) -> IO ()

setDeimage ::
     IsImage image
  => Window
  -> Maybe image
  -> IO ()
setDeimage box = \case
  Nothing ->
    Fltk.setDeimage (unWindow box) (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> Fltk.setDeimage (unWindow box) (Just ref))

setFlag ::
     Window
  -> Fltk.WidgetFlag
  -> IO ()
setFlag =
  wrapped Fltk.setFlag

-- setIcon:: (Parent a Image) => Ref WindowBase -> Maybe( Ref a ) -> IO ()

-- setIconlabel :: Ref WindowBase -> Text -> IO ()

setImage ::
     IsImage image
  => Window
  -> Maybe image
  -> IO ()
setImage box = \case
  Nothing ->
    Fltk.setImage (unWindow box) (Nothing @(Fltk.Ref Fltk.Image))
  Just image ->
    asImage image (\ref -> Fltk.setImage (unWindow box) (Just ref))

setLabel ::
     Window
  -> Text
  -> IO ()
setLabel =
  wrapped Fltk.setLabel

setLabelcolor ::
     Window
  -> Fltk.Color
  -> IO ()
setLabelcolor =
  wrapped Fltk.setLabelcolor

setLabelfont ::
     Window
  -> Fltk.Font
  -> IO ()
setLabelfont =
  wrapped Fltk.setLabelfont

setLabelsize ::
     Window
  -> Fltk.FontSize
  -> IO ()
setLabelsize =
  wrapped Fltk.setLabelsize

setLabeltype ::
     Window
  -> Fltk.Labeltype
  -> Fltk.ResolveImageLabelConflict
  -> IO ()
setLabeltype =
  wrapped Fltk.setLabeltype

-- setLabelWithIconlabel :: Ref WindowBase -> Text -> Text -> IO ()

-- setMenuWindow :: Ref WindowBase -> IO ()

-- setModal :: Ref WindowBase -> IO ()

-- setNonModal :: Ref WindowBase -> IO ()

setNotResizable ::
     Window
  -> IO ()
setNotResizable =
  wrapped Fltk.setNotResizable

setOutput ::
     Window
  -> IO ()
setOutput =
  wrapped Fltk.setOutput

-- setOverride :: Ref WindowBase -> IO ()

setParent ::
     IsGroupBase group
  => Window
  -> Maybe group
  -> IO ()
setParent box = \case
  Nothing ->
    Fltk.setParent (unWindow box) (Nothing @(Fltk.Ref Fltk.GroupBase))
  Just group ->
    asGroupBase group (\ref -> Fltk.setParent (unWindow box) (Just ref))

setResizable ::
     IsWidgetBase widget
  => Window
  -> Maybe widget
  -> IO ()
setResizable window = \case
  Nothing ->
    Fltk.setResizable (unWindow window) (Nothing @(Fltk.Ref Fltk.WidgetBase))
  Just widget ->
    asWidgetBase widget (\ref -> Fltk.setResizable (unWindow window) (Just ref))

setSelectionColor ::
     Window
  -> Fltk.Color
  -> IO ()
setSelectionColor =
  wrapped Fltk.setSelectionColor

setTooltip ::
     Window
  -> Text
  -> IO ()
setTooltip =
  wrapped Fltk.setTooltip

-- setTooltipWindow :: Ref WindowBase -> IO ()

setType ::
     Window
  -> Fltk.WindowType
  -> IO ()
setType =
  wrapped Fltk.setType

setVisible ::
     Window
  -> IO ()
setVisible =
  wrapped Fltk.setVisible

setVisibleFocus ::
     Window
  -> IO ()
setVisibleFocus =
  wrapped Fltk.setVisibleFocus

setWhen ::
     Window
  -> [Fltk.When]
  -> IO ()
setWhen =
  wrapped Fltk.setWhen

-- setXclass :: Ref WindowBase -> Text -> IO ()

-- shown :: Ref WindowBase -> IO (Bool)

showWidget ::
     Window
  -> IO ()
showWidget =
  wrapped Fltk.showWidget

-- sizeRange :: Ref WindowBase -> Size -> IO ()

-- sizeRangeWithArgs :: Ref WindowBase -> Size -> OptionalSizeRangeArgs -> IO ()

takeFocus ::
     Window
  -> IO (Either Fltk.NoChange ())
takeFocus =
  wrapped Fltk.takeFocus

takesevents ::
     Window
  -> IO Bool
takesevents =
  wrapped Fltk.takesevents

updateChild ::
     IsWidgetBase widget
  => Window
  -> widget
  -> IO ()
updateChild window widget =
  asWidgetBase widget (Fltk.updateChild (unWindow window))

-- waitForExpose :: Ref WindowBase -> IO ()

within ::
     Window
  -> IO a
  -> IO a
within =
  wrapped Fltk.within






