{-# LANGUAGE CPP #-}
module DToggleButtonF(
  HasLabelInside(..),ToggleButtonF,
  toggleButtonF,toggleButtonF' --,toggleButtonF''
  ) where
import FDefaults
import ToggleButtonF(oldToggleButtonF')
import NullF(F)
import Xtypes
import Defaults(buttonFont)
import Graphic
import GCAttrs --(FontSpec,fontSpec) -- + instances

#include "defaults.h"

newtype ToggleButtonF = Pars [Pars]
data Pars = LabelInside Bool | FontSpec FontSpec | Keys [(ModState, KeySym)]

parameter_class(LabelInside,Bool)

parameter_instance(LabelInside,ToggleButtonF)
parameter_instance(FontSpec,ToggleButtonF)
parameter_instance(Keys,ToggleButtonF)

toggleButtonF lbl = toggleButtonF' standard lbl

toggleButtonF' :: (Graphic lbl)=> Customiser ToggleButtonF -> lbl -> F Bool Bool
toggleButtonF' pmod lbl = 
    oldToggleButtonF' inside font keys lbl
  where
    inside = getLabelInside ps
    font   = getFontSpec ps
    keys   = getKeys ps
    ps     = pmod ps0
    ps0    =  Pars [LabelInside False,FontSpec (fontSpec buttonFont), Keys []]
