module MenuButtonF(menuButtonF,menuLabelF) where
import ButtonGroupF
import CompOps((>^^=<),(>=^<))
import SpEither(filterRightSP)
import SerCompF(idRightF)
import Defaults(inputFg,inputBg)
--import EitherUtils(stripEither)
import PushButtonF(Click(..))
import ResourceIds(FontName(..))--ColorName(..),
import Spacers(sepS)
import GraphicsF
import FDefaults
import DrawingUtils(g,boxD,spacedD)--fgD,fontD,
import Graphic
import Drawing(DPath)
import Geometry(pP)
import Fudget
import Sizing(Sizing(..))
import GCAttrs()

menuButtonF :: Graphic lbl => FontName -> lbl -> F lbl Click
menuButtonF fname label =
    filterRightSP >^^=< menuButtonGroupF (idRightF lblF >=^< prep)
  where
    lblF = menuLabelF fname label
    toDisp = Left
    through = Right
    prep (Left BMNormal) = toDisp (Left False)
    prep (Left BMInverted) = toDisp (Left True)
    prep (Left BMClick) = through Click
    prep (Right e) = toDisp (Right e)

menuLabelF :: Graphic lbl => FontName -> lbl -> F (Either Bool lbl) (GfxEvent DPath)
menuLabelF fname label =
    lblF >=^< pre
  where
    lblF = graphicsDispF' custom
    custom = setBgColor inputBg . setInitDisp (lblD label) .
	     setGfxEventMask [] . setSizing Dynamic . setBorderWidth 0 .
	     setFgColor inputFg . setFont fname
    lblD label = boxD [spacedD (sepS (pP 3 1)) $ g label]

    pre (Left highlight) = highlightGfx [] highlight
    pre (Right label')  = replaceAllGfx (lblD label')
