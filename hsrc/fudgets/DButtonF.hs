{-# LANGUAGE CPP #-}
module DButtonF(
  ButtonF,buttonF,buttonF',buttonF'',setLabel
  ) where
import FDefaults
import ButtonF(oldButtonF)
--import Fudget
--import Geometry(Rect)
import PushButtonF(Click)
import Xtypes
import Defaults(buttonFont,fgColor,bgColor)
import CmdLineEnv(argKeyList)
import CompOps((>^=<),(>=^^<))
--import Spops(concmapSP)
import SpEither(mapFilterSP)--filterRightSP
import EitherUtils(stripEither)
import SerCompF(idRightF)
import Spacers(Distance(..))
import Alignment(aCenter) --,Alignment(..)
import Graphic
import GCAttrs --(ColorSpec,colorSpec) -- + instances

#include "defaults.h"

newtype ButtonF lbl = Pars [Pars lbl]
data Pars lbl
  = FontSpec FontSpec
  | Keys [(ModState, KeySym)]
  | FgColorSpec ColorSpec
  | BgColorSpec ColorSpec
  | Margin Distance
  | Align Alignment
  | Label lbl

parameter_instance1(FontSpec,ButtonF)
parameter_instance1(Keys,ButtonF)
parameter_instance1(FgColorSpec,ButtonF)
parameter_instance1(BgColorSpec,ButtonF)
parameter_instance1(Margin,ButtonF)
parameter_instance1(Align,ButtonF)
parameter(Label)

buttonF s = buttonF' standard s
buttonF' pm s = noPF $ buttonF'' pm s

buttonF'' ::
  Graphic lbl => Customiser (ButtonF lbl) -> lbl -> PF (ButtonF lbl) Click Click
buttonF'' pmod s =
    stripEither >^=<
    idRightF (oldButtonF align marg font bg fg keys lbl >=^^< mapFilterSP relbl)
  where
    lbl  = getLabel ps
    font = getFontSpec ps
    keys = getKeys ps
    ps   = pmod ps0
    bg   = getBgColorSpec ps
    fg   = getFgColorSpec ps
    marg = getMargin ps
    align = getAlign ps
    ps0  = Pars [FontSpec (fontSpec buttonFont), Keys [],Margin 2,Align aCenter,
		 FgColorSpec buttonfg, BgColorSpec buttonbg, Label s]
    --relbl pmod' = [lbl | let Pars ps'=pmod' (Pars []), Label lbl<-ps']
    relbl pmod' = getLabelMaybe (pmod' (Pars []))

buttonbg = colorSpec (argKeyList "buttonbg" [bgColor,"white"])
buttonfg = colorSpec (argKeyList "buttonfg" [fgColor,"black"])
