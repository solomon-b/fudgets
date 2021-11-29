{-# LANGUAGE CPP #-}
module DRadioF(
  RadioGroupF,radioGroupF,radioGroupF',
  setPlacer
  ) where
import FDefaults
import RadioF(radioF)
import DToggleButtonF(HasLabelInside(..))
import NullF(F)
import LayoutRequest(Placer)
import Spacers() -- synonym Distance, for hbc
import Placers2(verticalLeftP')
--import Xtypes
import ResourceIds() -- synonym FontName, for hbc
import Defaults(buttonFont)
import Graphic
import GCAttrs --(FontSpec,fontSpec)

#include "defaults.h"

newtype RadioGroupF = Pars [Pars]
data Pars = LabelInside Bool | FontSpec FontSpec | Placer Placer

parameter(Placer)
parameter_instance(LabelInside,RadioGroupF)
parameter_instance(FontSpec,RadioGroupF)

radioGroupF lbl = radioGroupF' standard lbl

radioGroupF' :: (Graphic lbl,Eq alt )=> Customiser RadioGroupF -> [(alt,lbl)] -> alt -> F alt alt
radioGroupF' pmod alts startalt = 
    radioF placer inside font alts startalt
  where
    placer  = getPlacer ps
    inside  = getLabelInside ps
    font    = getFontSpec ps
    ps      = pmod ps0
    ps0     = Pars [LabelInside False,FontSpec (fontSpec buttonFont),Placer placer0]
    placer0 = verticalLeftP' 0
