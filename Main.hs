{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.StateVar                             (($=))
import Graphics.UI.FLTK.LowLevel.FL              (run)
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types

import qualified Fltk.Box    as Box
import qualified Fltk.Button as Button
import qualified Fltk.Window as Window


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

  Box.labelFont box $= helveticaBoldItalic
  Box.labelSize box $= (FontSize 36)
  Box.labelType box $= ShadowLabelType

  _ <-
    Button.new
      Button.Plain
        (Rectangle
          (Position (X 30) (Y 30))
          (Size (Width 100) (Height 50)))
        "Hello"

  _ <-
    Button.new
      Button.Check
        (Rectangle
          (Position (X 35) (Y 35))
          (Size (Width 100) (Height 50)))
        "World"

  Window.end window
  Window.visible window $= True

  run
