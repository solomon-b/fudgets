module Graphic2Pixmap(module Graphic2Pixmap,PixmapImage(..)) where
import DrawCompiledGraphics(drawK')
import MeasuredGraphics(compileMG)
import Graphic
import PixmapGen
import Pixmap(createPixmap)
import LayoutRequest(minsize)
import XDraw
import ResourceIds(copyFromParent)
import FixedDrawing
import FudgetIO
import NullF() -- instance FudgetIO K

graphic2PixmapImage g gctx cont =
  measureGraphicK g gctx $ \ mg -> measuredGraphics2Pixmap mg cont

measuredGraphics2Pixmap mg cont =
  let (cg,req) = compileMG id mg
      size = minsize req
  in createPixmap size copyFromParent $ \ pm ->
     drawK' (Pixmap pm) (undefined,const []) (const []) cg $
     cont (PixmapImage size pm)

instance PixmapGen FixedColorDrawing where
  convToPixmapK (FixCD size gcdcmds) cont =
    createPixmap size copyFromParent $ \ pm ->
    putLow (pmDrawMany pm gcdcmds) $
    cont (PixmapImage size pm)
