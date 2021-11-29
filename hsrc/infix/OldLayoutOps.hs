module OldLayoutOps((>==#<), (>+#<)) where
--import Fudget
import LayoutDir(Orientation(..))
import Placers(horizontalP',verticalP')
import Placer(placerF)
import AlignP(revP)
--import LayoutRequest
--import Geometry
import CompF(compF)
import SerCompF(serCompF)

infixl >+#<, >==#<

-- Old version of infix operators for common layout combinators.
-- Provided for backwards compatibility only.

f1 >+#<  of2 = cLF compF f1 of2
f1 >==#< of2 = cLF serCompF f1 of2

cLF cF f1 (dist,ori,f2) =
    let placer =
            case ori of
              Above -> verticalP'
              Below -> revP . verticalP'
              LeftOf -> horizontalP'
              RightOf -> revP . horizontalP'
    in placerF (placer dist) (cF f1 f2)
