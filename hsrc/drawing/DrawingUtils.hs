{-# LANGUAGE CPP #-}
module DrawingUtils where
import Xtypes
import Drawing
import Graphic
import FixedDrawing
import FlexibleDrawing(blank')
import GCAttrs
import GCtx
import Placers
import MatrixP(matrixP,matrixP')
import Spacers
import Placers2
import TableP(tableP,tableP')
import Geometry
import DrawTypes(DrawCommand(..))
import Alignment(aLeft,aCenter,aTop)
import LayoutDir(LayoutDir(..))
--import EitherUtils(Cont(..))
--import Fudget(K)

#include "exists.h"

boxVisibleD = ComposedD
boxD ds = ComposedD (length ds) ds
stackD = PlacedD overlayP . boxD
vertD = PlacedD verticalP
vertD' = PlacedD . verticalP'
horizD = PlacedD horizontalP
horizD' = PlacedD . horizontalP'
vboxD = vertD . boxD
hboxD = horizD . boxD
vboxD' sep = vertD' sep . boxD
hboxD' sep = horizD' sep . boxD

vertlD = PlacedD verticalLeftP
vertlD' = PlacedD . verticalLeftP'
vboxlD = vertlD . boxD
vboxlD' sep = vertlD' sep . boxD
horizcD = PlacedD horizontalCenterP
horizcD' = PlacedD . horizontalCenterP'
hboxcD = horizcD . boxD
hboxcD' sep = horizcD' sep . boxD

tableD n = PlacedD (tableP n) . boxD
tableD' sep n = PlacedD (tableP' n Horizontal sep) . boxD

matrixD n = PlacedD (matrixP n) . boxD
matrixD' sep n = PlacedD (matrixP' n Horizontal sep) . boxD

westD = spacedD $ hvAlignS aLeft aCenter
northwestD = spacedD $ hvAlignS aLeft aTop

padD = spacedD.marginS

fontD fn = softAttribD [GCFont (fontSpec fn)]

--fgnD :: ColorName -> Drawing lbl leaf -> Drawing lbl leaf
--fgnD = fgD.Name

--fontnD :: FontName -> Drawing lbl leaf -> Drawing lbl leaf
--fontnD = fontD.Name

fgD color = softAttribD [GCForeground (colorSpec color)]
bgD color = softAttribD [GCBackground (colorSpec color)]
fatD = softAttribD [GCLineWidth 5,GCCapStyle CapRound]
--attribD = belowAnnotD.AttribD --hmm
attribD = AttribD 
softAttribD = attribD.SoftGC
hardAttribD = attribD.HardGC
--spacedD = belowAnnotD.SpacedD  --hmm
spacedD = SpacedD

--belowAnnotD f (LabelD a d) = LabelD a (belowAnnotD f d)
--belowAnnotD f d = f d

#ifdef USE_EXIST_Q
data Gfx = EXISTS(a) TSTHACK((Graphic EQV(a)) =>) G EQV(a)
-- deriving Show -- doesn't work because of a HBC bug

instance Show Gfx where showsPrec n (G x) s = "G "++{-showsPrec 10 x-} s

instance Graphic Gfx where
  measureGraphicK (G x) = measureGraphicK x

g x = atomicD (G x)

#else

g = atomicD

#endif

filledRectD size = g (FixD (size) [FillRectangle (Rect origin size)])
rectD size = g (FixD (size+1) [DrawRectangle (Rect origin size)])
                   -- size+1 assumes that the line width is 1
blankD = g . blank' 

holeD =
    fgD "blue3" $ rectD size
    --stack [fgD "white" (filledRectD size),rectD size]
  where
    size = pP 15 13
