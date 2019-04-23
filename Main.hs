{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

  Box.setLabelfont box helveticaBoldItalic
  Box.setLabelsize box (FontSize 36)
  Box.setLabeltype box ShadowLabelType ResolveImageLabelOverwrite

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
  Window.showWidget window

  run
