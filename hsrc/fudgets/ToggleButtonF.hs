module ToggleButtonF(toggleF, oldToggleButtonF, oldToggleButtonF') where
--import Alignment
import Spacer(marginF)
import CompOps((>=^<), (>^=<),(>+<),(>=^^<))
import CompSP(idRightSP)
import SpEither(filterRightSP)
import Spacers(compS,vCenterS,noStretchS)--,centerS,hvAlignS,spacerP
--import Placers(horizontalP)
import Placer(hBoxF,spacer1F)
import EitherUtils(stripEither)
import OnOffDispF
import ButtonBorderF
import ToggleGroupF
--import Geometry(Point(..))
import GraphicsF(graphicsLabelF')
import Defaults(look3d, edgeWidth)
import FDefaults -- setFont

toggleF inside keys lblF =
   toggleGroupF keys $
   if inside
   then buttonBorderF edgew (mF lblF) >=^^< idRightSP filterRightSP
   else stripEither >^=< hBoxF (sF (buttonBorderF edgew indicatorF) >+< cF lblF)
 where
    indicatorF = mF (onOffDispF False)
    mF = marginF innersep
    sF = spacer1F (noStretchS True True `compS` vCenterS)
    cF = spacer1F (noStretchS False True `compS` vCenterS)
    innersep = 2
    edgew    = if look3d then edgeWidth else max 0 (edgeWidth-1)

oldToggleButtonF x = oldToggleButtonF' False x

oldToggleButtonF' inside fname keys lbl =
    stripEither >^=< toggleF inside keys lblF >=^< Left
  where
    lblF = graphicsLabelF' (setFont fname) lbl
