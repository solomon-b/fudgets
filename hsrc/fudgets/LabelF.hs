module LabelF(labRightOfF, labLeftOfF, labBelowF, labAboveF, tieLabelF) where
import Spacer(noStretchF,marginHVAlignF)
import Alignment
import CompOps((>=^<), (>^=<))
import DDisplayF(labelF)
--import Fudget
--import Geometry
import LayoutDir(Orientation(..))
import LayoutOps
import EitherUtils(stripEither)
--import Xtypes

--tieLabelF :: Orientation -> Alignment -> String -> F a b -> F a b
tieLabelF orient align text fudget =
    let disp = labelF text
        fv = orient == Above || orient == Below
        fh = not fv
        lblF = noStretchF fh fv (marginHVAlignF 0 align align disp)
    in  (stripEither >^=< ((lblF,orient) >#+< fudget)) >=^< Right


labF orient = tieLabelF orient aCenter
labAboveF   x = labF Above x
labBelowF   x = labF Below x
labLeftOfF  x = labF LeftOf x
labRightOfF x = labF RightOf x

-- eta expanded because of monomorphism restriction
