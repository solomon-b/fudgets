import AllFudgets
import Data.Array
import Mandelbrot
import ZoomF
import UserCoords
import Control.Parallel
--import Trace

main = fudlogue $ shellF "Mandelbrot Explorer" mainF

mainF =
    allocColorScale $ \ colors ->
    loopCompThroughLeftF (zoomF (dispF colors >=^^< preSP)) >==< resetF
  where
    dispF colors = nullSP>^^=<graphicsDispF >=^< replaceAllGfx . image colors
    resetF = const area0>^=<buttonF "Reset"
    preSP = putSP area0 $ mapstateSP zoom area0
    zoom area = either zoom reset
      where
        reset area' = new area'
	zoom msg =
	  case msg of
	    ZoomIn s p  -> new (zoomin s p area)
	    ZoomOut s p -> new (zoomout s p area)
	    ZoomRect size (Rect p s) ->
	      if s =.> 2
	      then new (zoomrect (size,area) p (p+s))
	      else same
	    _ -> same
	same = (area,[])
	new area' = (area',[area'])

--sizeF = "Size:" `labLeftOfF` startupF [100] intInputF

image :: Array Int Pixel -> Area -> Drawing nothing FlexibleDrawing
image grays area = atomicD (FlexD 768 False False drawfunc)
  where
    drawfunc (Rect p size0@(Point w0 h0)) =
        stripes 5 (CreatePutImage (Rect p size) zPixmap pixels)
      where
	w = min w0 h0
	size = diag w -- keep it square
	pixels = {-parallel-} [grays!g (Point ix iy) | iy <- [0..w-1], ix<-[0..w-1]]
	g = pickColor . mandelbrot . userp (size,area)
	pickColor z = z `mod` colorCount
	rw = real w

-- Color department

-- We interpolate a scale of colors from two specified end colors
allocColorScale return =
    allocNamedColor defaultColormap color1 $ \ (Color _ rgb1) ->
    allocNamedColor defaultColormap color2 $ \ (Color _ rgb2) ->
    conts (allocColorPixelF defaultColormap) (rgbs rgb1 rgb2) $
    return . listArray (0,high)
  where
    rgbs rgb1 rgb2 = [mixRGB rgb1 rgb2 i | i<-[0..high]]
    high = colorCount-1
    mixRGB (RGB r1 g1 b1) (RGB r2 g2 b2) k = RGB (m r1 r2) (m g1 g2) (m b1 b2)
      where m i1 i2 = (i1*(high-k) + i2*k) `div` high

colorCount = argReadKey "colorcount" 16 :: Int
color1 = argKey "color1" "navyblue"
color2 = argKey "color2" "white"

-- Extra

-- stripes splits one big CreatePutImage command into a number of smaller
-- so you don't have wait for the entire image to be computed before you
-- can see anything.
stripes sh dcmd@(CreatePutImage (Rect p (Point w h)) fmt pxls) =
  if h<=sh
  then [dcmd]
  else let rect' = Rect (p+(Point 0 sh)) (Point w (h-sh))
	   (pxls1,pxls2) = splitAt (w*sh) pxls
           ss = stripes sh (CreatePutImage rect' fmt pxls2)
       in par ss $ par (pxls1==pxls1) $
          CreatePutImage (Rect p (Point w sh)) fmt pxls1 :
          ss

parallel [] = []
parallel xxs@(x:xs) = seq (parallel xs) $ par x xxs
