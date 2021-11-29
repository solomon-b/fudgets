module Placer where
import Fudget
import LayoutRequest
import FRequest
--import Geometry(Point, Size(..),Rect)
import NullF(putMessageFu)
--import Command
--import Defaults(defaultSep)
import Placers(horizontalP,verticalP)
import MatrixP(matrixP)
import Spacers(spacerP)
import TableP(tableP)
import AlignP(revP)
import AutoPlacer(autoP)

--import AlignP
--import Alignment

placerF :: Placer -> F a b -> F a b
placerF placer = putMessageFu (Low (LCmd (LayoutPlacer placer)))

spacerF :: Spacer -> F a b -> F a b
spacerF spacer = putMessageFu (Low (LCmd (LayoutSpacer spacer)))

spacer1F s = placerF (s `spacerP` autoP)

hBoxF = placerF horizontalP
vBoxF = placerF verticalP

revHBoxF = placerF (revP horizontalP)
revVBoxF = placerF (revP verticalP)

matrixF = placerF . matrixP
tableF  = placerF . tableP
