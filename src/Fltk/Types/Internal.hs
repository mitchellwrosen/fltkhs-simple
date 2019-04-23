-- | All the types, to avoid cyclic imports.

{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Fltk.Types.Internal where

import qualified Graphics.UI.FLTK.LowLevel.Dispatch  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Fl_Types  as Fltk
import qualified Graphics.UI.FLTK.LowLevel.Hierarchy as Fltk


-- | A 'Box' is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Box.html>
newtype Box
  = Box { unBox :: Fltk.Ref Fltk.Box }


-- | A 'Button' is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Button.html>
newtype Button
  = Button { unButton :: Fltk.Ref Fltk.ButtonBase }

instance IsWidget Button where asWidget button f = f (unButton button)


-- | A 'Group' is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Group.html>
newtype Group
  = Group { unGroup :: Fltk.Ref Fltk.GroupBase }

instance IsGroup  Group where asGroup  group f = f (unGroup group)
instance IsWidget Group where asWidget group f = f (unGroup group)


-- | <https://www.fltk.org/doc-1.4/classFl__Image.html>
newtype Image
  = Image { unImage :: Fltk.Ref Fltk.Image }

instance IsImage Image where asImage image f = f (unImage image)


-- | An 'Input' is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Input.html>
newtype Input
  = Input { unInput :: Fltk.Ref Fltk.InputBase }

instance IsWidget Input where asWidget input f = f (unInput input)


-- | <https://www.fltk.org/doc-1.4/classFl__Widget.html>
newtype Widget
  = Widget { unWidget :: Fltk.Ref Fltk.WidgetBase }

instance IsWidget Widget where asWidget widget f = f (unWidget widget)


-- | A 'Window' is a 'Fltk.Group.Group', which is a 'Fltk.Widget.Widget'.
--
-- <https://www.fltk.org/doc-1.4/classFl__Window.html>
newtype Window
  = Window { unWindow :: Fltk.Ref Fltk.WindowBase }

instance IsGroup  Window where asGroup  window f = f (unWindow window)
instance IsWidget Window where asWidget window f = f (unWindow window)


-- | 'Fltk.Group.Group' and its subclasses.
class IsGroup a where
  asGroup ::
       a
    -> (forall b. Fltk.Parent b Fltk.GroupBase => Fltk.Ref b -> r)
    -> r

-- | 'Fltk.Image.Image' and its subclasses.
class IsImage a where
  asImage ::
       a
    -> (forall b. Fltk.Parent b Fltk.Image => Fltk.Ref b -> r)
    -> r

-- | 'Fltk.Widget.Widget' and its subclasses.
class IsWidget a where
  asWidget ::
       a
    -> (forall b. Fltk.Parent b Fltk.WidgetBase => Fltk.Ref b -> r)
    -> r

-- | 'Fltk.Window.Window' and its subclasses.
class IsWindow a where
  asWindow ::
       a
    -> (forall b. Fltk.Parent b Fltk.WindowBase => Fltk.Ref b -> r)
    -> r
