module Graphic(module Graphic,MeasuredGraphics,emptyMG,emptyMG',GCtx,Cont(..)) where
import Fudget
import EitherUtils(Cont(..))
import Cont(conts)
import MeasuredGraphics(MeasuredGraphics(..),measureString,measurePackedString,emptyMG,emptyMG')
import GCtx(GCtx)
import PackedString(PackedString)
import Geometry() -- instance Num Point

class Graphic a where
  measureGraphicK :: a -> GCtx -> Cont (K i o) MeasuredGraphics
  measureGraphicListK :: [a] -> GCtx -> Cont (K i o) MeasuredGraphics
  -- Default method for lists:
  measureGraphicListK xs gctx cont =
	conts (`measureGraphicK` gctx) xs $ \ mgs ->
	cont (ComposedM mgs)

instance Graphic MeasuredGraphics where
  measureGraphicK cgfx gctx c = c cgfx

instance Graphic Char where
  measureGraphicK c = measureString [c]
  measureGraphicListK = measureString

instance Graphic a => Graphic [a] where
  measureGraphicK = measureGraphicListK

instance (Graphic a,Graphic b) => Graphic (a,b) where
  measureGraphicK (x,y) gctx cont =
    measureGraphicK x gctx $ \ mx ->
    measureGraphicK y gctx $ \ my ->
    cont (ComposedM [mx,my])

measureText x = (measureString.show) x

-- instance Text a => Graphics a where measureGraphicK = measureText
instance Graphic Int          where measureGraphicK = measureText
instance Graphic Integer      where measureGraphicK = measureText
instance Graphic Bool         where measureGraphicK = measureText
instance Graphic Float        where measureGraphicK = measureText
instance Graphic Double       where measureGraphicK = measureText
instance Graphic PackedString where measureGraphicK = measurePackedString

instance (Graphic a,Graphic b) => Graphic (Either a b) where
  measureGraphicK = either measureGraphicK measureGraphicK

instance Graphic a => Graphic (Maybe a) where
  measureGraphicK = maybe (measureGraphicK (emptyMG 5)) measureGraphicK

--instance Graphic Void
