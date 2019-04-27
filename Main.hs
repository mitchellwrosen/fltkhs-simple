{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Data.StateVar                             (($=))
import Fltk
import Graphics.UI.FLTK.LowLevel.FL              (run)
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types

import qualified Fltk.Widget.Box          as Box
import qualified Fltk.Widget.Button       as Button
import qualified Fltk.Widget.Group.Window as Window


main :: IO Int
main = do
  window <-
    Window.new
      (Size (Width 340) (Height 180))
      Nothing
      "Hello"

  box <-
    Box.new
      UpBox
      (Rectangle
        (Position (X 20) (Y 40))
        (Size (Width 300) (Height 100)))
      "Hello, world!"

  Box.labelStyle box $=
    LabelStyle blackColor helveticaBoldItalic (FontSize 36) ShadowLabelType

  -- _ <-
  --   Button.new
  --     Button.Plain
  --       (Rectangle
  --         (Position (X 30) (Y 30))
  --         (Size (Width 100) (Height 50)))
  --       "Hello"

  -- _ <-
  --   Button.new
  --     Button.Check
  --       (Rectangle
  --         (Position (X 35) (Y 35))
  --         (Size (Width 100) (Height 50)))
  --       "World"

  Window.end window

  Window.visible window $= True

  -- Window.drawBox window

  run
