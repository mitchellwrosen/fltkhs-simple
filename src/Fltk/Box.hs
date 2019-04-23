module Fltk.Box
  ( Box
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
  , copyTooltip
  , deactivate
  , destroy
  , doCallback
  , drawBackdrop
  , drawBox
  , drawBoxWithBoxtype
  , drawFocus
  , drawLabel
  , flags
  , getAlign
  , getBox
  , getCallback
  , getColor
  , getDamage
  , getDeimage
  , getH
  , getImage
  , getLabel
  , getLabelcolor
  , getLabelfont
  , getLabelsize
  , getLabeltype
  , getOutput
  , getParent
  , getRectangle
  , getSelectionColor
  , getTooltip
  , getTopWindow
  , getTopWindowOffset
  , getType_
  , getVisible
  , getVisibleFocus
  , getVisibleR
  , getW
  , getWhen
  , getWindow
  , getX
  , getY
  , handle
  , hasCallback
  , hide
  , inside
  , measureLabel
  , modifyVisibleFocus
  , redraw
  , redrawLabel
  , resize
  , setActive
  , setAlign
  , setBox
  , setCallback
  , setChanged
  , setColor
  , setColorWithBgSel
  , setDamage
  , setDamageInside
  , setDeimage
  , setFlag
  , setImage
  , setLabel
  , setLabelcolor
  , setLabelfont
  , setLabelsize
  , setLabeltype
  , setOutput
  , setParent
  , setSelectionColor
  , setTooltip
  , setType
  , setVisible
  , setVisibleFocus
  , setWhen
  , showWidget
  , takeFocus
  , takesevents
  ) where

import Fltk.Box.Internal
