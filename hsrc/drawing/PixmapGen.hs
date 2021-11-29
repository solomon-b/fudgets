module PixmapGen where
import Graphic
import MeasuredGraphics(MeasuredGraphics(..))
import GCtx(GCtx(..))
--import Command
--import Event
import DrawTypes
import Geometry(Rect(..),Size,origin)
import Xtypes
import FudgetIO
import NullF() -- instances, for hbc
import LayoutRequest(plainLayout)
import Gc
--import EitherUtils(Cont(..))
--import Io(appendChanK)
--import Pixmap(readBitmapFile)
--import Maybe(maybeToList)
--import ContinuationIO(stderr)

data PixmapImage = PixmapImage Size PixmapId

instance Graphic PixmapImage where
    measureGraphicK (PixmapImage size pixmap) (GC gc _) k =
      wCreateGC gc [GCGraphicsExposures False] $ \ gc' ->
      let r = Rect origin size
	  ll = plainLayout size True True
	  drawit (Rect p _) = [(gc,[CopyArea (Pixmap pixmap) r p])]
      in k (LeafM ll drawit)

---

class PixmapGen a where
  convToPixmapK :: FudgetIO c => a -> Cont (c i o) PixmapImage

measureImageK a gctx k =
  convToPixmapK a $ \ pmi ->
  measureGraphicK pmi gctx k

--This is not allowed in Haskell...
--instance PixmapGen a => Graphic a where  measureGraphicsK = measureImageK

instance PixmapGen PixmapImage where
  convToPixmapK pmi k = k pmi
