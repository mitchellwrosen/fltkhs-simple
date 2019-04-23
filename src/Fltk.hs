module Fltk
  ( Upcast(..)
  ) where

class Upcast a b where
  upcast :: a -> b
