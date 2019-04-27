module Fltk.MenuItem
  ( -- * MenuItem
    MenuItem
  , new
    -- ** Read/write properties
  , active
  , label
  , labelStyle
  , shortcut
  , visible
  , value
    -- ** Read-only queries
  , checkbox
  , findShortcut
  , radio
  , submenu
    -- ** Effectful functions
  , destroy
  , doCallback
  , setCallback
  ) where

import Fltk.Internal.Types (LabelStyle(..), MenuItem(..), Widget(..))

import qualified Fltk.Internal as Internal

import Data.Coerce   (coerce)
import Data.StateVar (StateVar)
import Data.Text     (Text)

import qualified Graphics.UI.FLTK.LowLevel.Base.MenuItem as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Dispatch      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types      as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy     as Fltk


new ::
    IO MenuItem
new =
  MenuItem . Fltk.safeCast <$> Fltk.menuItemNew

wrapped ::
     (Fltk.Ref Fltk.MenuItemBase -> a)
  -> MenuItem
  -> a
wrapped =
  coerce


--------------------------------------------------------------------------------
-- Read-write properties
--------------------------------------------------------------------------------

active ::
     MenuItem -- ^
  -> StateVar Bool
active =
  wrapped Internal.active

label ::
     MenuItem -- ^
  -> StateVar Text
label =
  wrapped Internal.label

labelStyle ::
     MenuItem -- ^
  -> StateVar LabelStyle
labelStyle =
  wrapped Internal.labelStyle2

shortcut ::
     MenuItem -- ^
  -> StateVar (Maybe Fltk.ShortcutKeySequence)
shortcut =
  wrapped Internal.shortcut

value ::
     MenuItem -- ^
  -> StateVar Int
value =
  wrapped Internal.value2

visible ::
     MenuItem -- ^
  -> StateVar Bool
visible =
  wrapped Internal.visible2


--------------------------------------------------------------------------------
-- Read-only queries
--------------------------------------------------------------------------------

checkbox ::
     MenuItem -- ^
  -> IO Bool
checkbox =
  wrapped Fltk.checkbox

findShortcut ::
     MenuItem -- ^
  -> Maybe Fltk.AtIndex -- ^
  -> Bool -- ^
  -> IO (Maybe MenuItem)
findShortcut item index requireAlt =
  coerce @(IO (Maybe (Fltk.Ref Fltk.MenuItemBase)))
    (wrapped Fltk.findShortcut item index requireAlt)

radio ::
     MenuItem -- ^
  -> IO Bool
radio =
  wrapped Fltk.radio

submenu ::
     MenuItem -- ^
  -> IO Bool
submenu =
  wrapped Fltk.submenu

--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

destroy ::
     MenuItem -- ^
  -> IO ()
destroy =
  wrapped Fltk.destroy

doCallback ::
     MenuItem -- ^
  -> Widget -- ^
  -> IO ()
doCallback item widget =
  wrapped Fltk.doCallback item (coerce widget)

setCallback ::
     MenuItem -- ^
  -> IO ()
  -> IO ()
setCallback item callback =
  wrapped Fltk.setCallback item (const callback)
