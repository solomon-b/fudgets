module FixedDrawing where
import Graphic
import MeasuredGraphics(MeasuredGraphics(..),measureImageString)
import GCtx(GCtx(..))
import ResourceIds(GCId)
import LayoutRequest
import Geometry(Rect(..),Size(..))
import Command(DrawCommand)
import Drawcmd(move)
--import EitherUtils(Cont(..))

data FixedDrawing = FixD Size [DrawCommand] deriving Show

instance Graphic FixedDrawing where
  measureGraphicK (FixD s dcmds) (GC gc _) k =
      k (LeafM (plainLayout s True True) drawit)
    where
      drawit (Rect p _) = [(gc,move p dcmds)]

data FixedColorDrawing = FixCD Size [(GCId,[DrawCommand])] deriving Show

instance Graphic FixedColorDrawing where
  measureGraphicK (FixCD s gcdcmds) _ k =
      k (LeafM (plainLayout s True True) drawit)
    where
      drawit (Rect p _) =
        if p==0
	then gcdcmds
	else [(gc,move p dcmds)|(gc,dcmds)<-gcdcmds]


newtype ImageString = ImageString String

instance Graphic ImageString where
  measureGraphicK (ImageString s) = measureImageString s
