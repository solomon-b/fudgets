module ShapeK(dynShapeK, shapeK) where
import Command(XCommand(FreeGC,FreePixmap,ShapeCombineMask,DrawMany))
import XDraw
import CompFfun(prepostMapHigh')
import Convgc
--import Event
import LayoutRequest(LayoutResponse(..))
import FRequest
import Gc
import Fudget
--import FudgetIO
import Xcommand
import NullF
import ParK
import Pixmap
import EitherUtils(stripEither)
import Data.Maybe(fromJust)
import Geometry(pP,Rect(..),origin,Size)
import Xtypes

dynShapeK gcattrs shapeCmds f = compK (shapeK1 gcattrs shapeCmds) f

shapeK :: (Size -> [DrawCommand]) -> K a b -> K a b
shapeK shapeCmds f =
    K{-kk-} (prepostMapHigh' Right stripEither dk)
  where
    K dk = dynShapeK [] shapeCmds f

shapeK1 gcattrs shape =
    convGCattrsK gcattrs (\gcattrs' -> shapeP gcattrs' shape Nothing)

shapeP gcattrs shape size =
    let reshape shape' size' =
          createPixmap size' 1 $ \pm ->
	  pmCreateGC pm rootGC [GCFunction GXcopy, GCForeground pixel0] $ \gcclr ->
	  pmCreateGC pm gcclr (GCForeground pixel1 :
                               GCBackground pixel0 :
                               gcattrs) $ \gc ->
	  xcommandsK [drawshape pm size' shape' gc gcclr,
	              ShapeCombineMask ShapeBounding (pP 0 0) pm ShapeSet,
		      FreePixmap pm,
		      FreeGC gcclr,
		      FreeGC gc] $
	  shapeP gcattrs shape' (Just size')
    in getK $ \msg ->
       case msg of
         Low (LEvt (LayoutSize size')) -> reshape shape size'
	 High shape' | size /= Nothing -> reshape shape' (fromJust size)
	 _ -> shapeP gcattrs shape size

drawshape pm size shapeCmds gc gcclr =
    DrawMany (Pixmap pm) [
      (gcclr,[FillRectangle (Rect origin size)]),
      (gc,shapeCmds size)]

