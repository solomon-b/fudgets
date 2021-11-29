module PlotF
    (plotF,ToPlot(..),Area(..),Interval(..),Func(..))
  where
import AllFudgets
import Data.List(sortBy)
import Data.Maybe(fromMaybe)
import ZoomF
import HbcUtils(apSnd)
import UserCoords
import Exp(Approx(..))

data Plot = Grid | Axes | Functions [Double->Double] | Lines [(Double,Double)]

plotReal size0 area plot = FlexD size0 False False draw
  where
    draw (Rect p size) = move p (drawgraph size area plot)

gridstep w xa h ya = max (gridstep' w xa) (gridstep' h (swap ya))
  where
    gridstep' w (x0,x1) =
       if mag>=minstep
       then mag
       else if mag2>=minstep
            then mag2
	    else if mag5>=minstep
	         then mag5
		 else mag10
      where
        maxcnt = w `div` 8
	minstep = d / fromIntegral maxcnt
	mag = 10^^floor (log10 minstep)
	mag2 = 2*mag
	mag5 = 5*mag
	mag10 = 10*mag
        d = x1-x0


drawgraph (Point w h) (xa,ya) plot =
    case plot of
      Grid -> drawgrid
        where
	  drawgrid = hlines++vlines
	  d = gridstep w xa h ya
	  hlines = [DrawLine (lL 0 y xh y) |
	            let lo=ceiling' d (snd ya),
		    yu<-[lo,lo+d .. floor' d (fst ya)],
		    let y=wdw h ya yu]
	  vlines = [DrawLine (lL x 0 x yh) |
	            let lo=ceiling' d (fst xa),
		    xu<-[lo,lo+d .. floor' d (snd xa)],
		    let x=wdw w xa xu]
      Axes -> drawaxes
        where
	  drawaxes =
	     [DrawLine (lL 0 y0 xh y0),
	      drawLines [pP (xh-d) (y0-d),pP xh y0,pP (xh-d) (y0+d)],
	      DrawLine (lL x1 (y0-d) x1 (y0+d)),
	      DrawLine (lL x0 0 x0 yh),
	      drawLines [pP (x0-d) d,pP x0 0,pP (x0+d) d],
	      DrawLine (lL (x0-d) y1 (x0+d) y1)]
      Functions fs -> map drawfunc fs 
	where
	  drawfunc f =
	    drawLines [Point wx ((wdw h ya . f . user w xa) wx) | wx<-[0,2..w]]
      Lines ps -> [drawLines [Point (wdw w xa x) (wdw h ya y)|(x,y)<-ps]]
  where
    drawLines = DrawLines CoordModeOrigin
    y0 = wdw h ya 0.0
    y1 = wdw h ya 1.0
    x0 = wdw w xa 0.0
    x1 = wdw w xa 1.0
    yh = h-1
    xh = w-1
    d = 4

ceiling' d x = fromIntegral (ceiling (x/d)) * d
floor' d x = fromIntegral (floor (x/d)) * d
log10 x = log x / log 10

showroot (Point w h) ((xa @ (x1,x2)),ya) roots p@(Point wx wy)=
    case sortroots [x | root<-roots, Just x<-[root (user w xa wx) eps]] of
      x:_ -> Popup (pP (wdw w xa x) (wdw h ya 0.0)) ("x=" ++ show x)
      [] -> Popup p "No root?"
  where
    sortroots = sortBy cmpdist
    cmpdist x1 x2 = compare (dist x1) (dist x2)
    dist x = abs (x-x0)
    x0 = user w xa wx
    eps = abs (x0/1.0e10)
    rnd x = real (round (x * r)) / r  -- keep approx 5 significant digits
      where r = scaleFloat (17+exponent x) 0.5

rmroot = Popdown

data ToPlot
  = RFunc Func
  | RLines Approx Int (Double->Double)

type Func = (Double->Double,Double->Double->Maybe Double)

zoomDispF size = zoomF dispF
  where
    dispF = graphicsDispGroupF' custom rootDispF
    custom = setInitDisp initD . setSizing Static .
             (setBgColor paper :: (Customiser (GraphicsF a)))
    initD = plotD size startarea []

plotD size area@(xa,_) fs =
    stackD (fgD grid (g gr):
            softAttribD (gcattrs fgColor) (g axes):
            ps)
  where
    ps = zipWith p1 (cycle pens) fs
    p1 pen f = softAttribD (gcattrs pen) (g (p f))
    p :: ToPlot -> FlexibleDrawing
    p (RFunc (f,_)) = plotReal size area (Functions [f])
    p (RLines Trapets n f) = plotReal size area (Lines (trapets xa n f))
    p (RLines RectO n f) = plotReal size area (Lines (rect max xa n f))
    p (RLines RectU n f) = plotReal size area (Lines (rect min xa n f))
    gr = plotReal size area Grid
    axes = plotReal size area Axes
    gcattrs fg = [GCForeground (colorSpec fg), GCLineWidth lw,
		  GCCapStyle CapProjecting
		  ]

    rect :: (Double->Double->Double)->(Double,Double) -> Int -> (Double->Double) -> [(Double,Double)]
    rect est xa n = vlines . hlines . trapets xa n
     where
      hlines ((x1,y1):ps@((x2,y2):_)) = (x1,x2,est y1 y2):hlines ps
      hlines _ = []

      vlines [] = []
      vlines ((x1,x2,y):hs) = (x1,y):(x2,y):vlines' y hs

      vlines' y [] = []
      vlines' y ((x1,x2,y2):hs) =
        if y==y2
	then (x2,y2):vlines' y2 hs
	else (x1,y2):(x2,y2):vlines' y2 hs

    trapets :: (Double,Double) -> Int -> (Double->Double) -> [(Double,Double)]
    trapets (x1,x2) n f = [pairwith f (x i)|i<-[0..n]]
      where
	x i = x1+dx*fromIntegral i
	dx = (x2-x1)/fromIntegral n

startarea = ((-6.5,6.5),(6.5,-6.5))

--plotF :: Maybe Size -> F (Either [Func] Area) (Either Area (Double,Double))
plotF optsize =
    loopThroughRightF ctrlF (zoomDispF size)
  where
    size = fromMaybe (Point 400 400) optsize

    --f0 x = 0.0 -- dummy
    --r0 x dx = Just x -- dummy
    state0 = (startarea,[])

    plot area funcs = replaceAllGfx $ plotD size area funcs

    toGfx = Left . Right
    toDisp = toGfx . Left
    toRootDisp = toGfx . Right

    ctrlF = putF (Right (Left startarea)) $ mapstateF ctrl state0

    ctrl state@(area,func) = either fromLoop fromOutsize
      where
        output x  = apSnd (Right x:)
        same = (state,[])
	redraw area' func' =
	  ((area',func'),[toRootDisp rmroot,toDisp (plot area' func')])
	zoom area' = output (Left area') $ redraw area' func
	newfunc f = redraw area f

	fromOutsize = either newfunc zoom
        fromLoop = either fromZoom fromGfx
	fromGfx _ = same

	fromZoom zoomMsg =
	  case zoomMsg of
	    ZoomIn s p  -> zoom (zoomin s p area)
	    ZoomOut s p -> zoom (zoomout s p area)
	    ZoomRect size (Rect p s) ->
	      if s =.> 2
	      then zoom (zoomrect (size,area) p (p+s))
	      else same
	    ZoomPick s p ->
	      (state,[toRootDisp (showroot s area [r|RFunc (_,r)<-func] p)])
	    ZoomResize _ -> (state,[toRootDisp rmroot])

rootDispF =
    --delayF $
    bubblePopupF (displayF' pm)
  where
    pm = setBorderWidth 0.setMargin 0.setSizing Dynamic. setBgColor "white"

pens = argKeyList "pens" ["blue3","red3","green4","orange"]
grid = argKey "grid" "grey"
paper = argKey "plotpaper" "grey90"
lw = argReadKey "linewidth" 2 -- 0 looks better than 1.
