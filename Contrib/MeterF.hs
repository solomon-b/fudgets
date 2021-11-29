module MeterF where
import AllFudgets
import Data.Ratio

meterF :: RealFrac v => InF v (Ratio Int) -- because of the monomorphism restr
meterF = meterF' standard

meterF' pm =
   post >^^=< border3dF True 1 (graphicsDispF' params) >=^< pre
 where
   params = pm .
	    --setSizing Static .
	    --setGfxEventMask [GfxButtonMask,GfxDragMask] .
	    setBgColor meterBg .
	    setFgColorSpec meterFg .
	    setBorderWidth 0
   pre = Right . replaceAllGfx . meterD
   post = mapFilterSP pick
   pick (GfxButtonEvent {gfxType=Pressed,gfxPaths=p:_}) = change p
   pick (GfxButtonEvent {gfxType=Released,gfxPaths=p:_}) = done p
   pick (GfxMotionEvent {gfxPaths=p:_}) = change p
   pick _ = Nothing
   change = Just . inputChange . extr
   done = Just . inputMsg . extr
   extr (_,(p,Rect p0 s)) = xcoord (p-p0) % xcoord s

meterD r = FlexD (Point 50 5) False True drawfun
  where
    drawfun (Rect p (Point w h)) = [FillRectangle (Rect p (Point w' h))]
      where w' = scale r w

meterFg = colorSpec $ argKeyList "meterfg" ["blue2",fgColor,"black"]
meterBg = colorSpec $ argKeyList "meterbg" [bgColor,"white"]
