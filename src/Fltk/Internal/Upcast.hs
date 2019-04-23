module Fltk.Internal.Upcast
  ( Upcast(..)
  ) where

class Upcast a b where
  upcast :: a -> b
