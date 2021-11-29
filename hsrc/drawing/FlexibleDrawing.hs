module FlexibleDrawing where
import Geometry
import DrawTypes
--import Xtypes(CoordMode(..),Shape(..))
import LayoutRequest
--import EitherUtils(Cont(..))
import Utils(aboth)
import Graphic
import MeasuredGraphics(MeasuredGraphics(..))
import GCtx(GCtx(..))

data FlexibleDrawing = FlexD Size Bool Bool (Rect->[DrawCommand]) deriving Show

instance Graphic FlexibleDrawing where
  measureGraphicK (FlexD s fh fv drawf) (GC gc _) k =
      k (LeafM (plainLayout s fh fv) drawf')
    where drawf' r = [(gc,drawf r)]

filler fh fv d = FlexD (diag d) fh fv (\r->[FillRectangle r])

--filledRect = filler False False
hFiller = filler False True
vFiller = filler True False

flex' s = FlexD s False False
flex = flex' 5

blank' s = flex' s (const [])
blank = blank' 5

frame' s  = flex' s (\r->[DrawRectangle (r `growrect` (-1))])
frame     = frame' 5

ellipse = ellipse' 5
ellipse' s = arc' s 0 (360*64)
arc = arc' 5
arc' s a1 a2 = flex' s (drawarc a1 a2)

filledEllipse = filledEllipse' 5
filledEllipse' s = filledarc' s 0 (360*64)
filledarc = filledarc' 5
filledarc' s a1 a2 = flex' s (fillarc a1 a2)

drawarc a1 a2 r = [DrawArc (r `growrect` (-1)) a1 a2]
fillarc a1 a2 r = [FillArc (r `growrect` (-1)) a1 a2]

rpar = bFlex (drawarc (-60*64) (120*64).doubleleft)
lpar = bFlex (drawarc (120*64) (120*64).doubleright)

doubleleft (Rect p s@(Point w _)) = Rect (p-d) (s+d) where d=Point w 0
doubleright (Rect p s@(Point w _)) = Rect p (s+d) where d=Point w 0

-- top level pattern bindings (with pbu) can't be trusted...
lbrack = fst bracks
rbrack = snd bracks

bracks = aboth bFlex (draw False, draw True)
  where
    draw right r = [DrawLine (Line p1 p2) | (p1,p2) <- ls ]
      where (p1,p2,p3,p4) = corners (r `moverect` 1 `growrect` (-2))
            ls = if right
	         then [(p1,p2),(p2,p4),(p3,p4)]
		 else [(p1,p2),(p1,p3),(p3,p4)]

corners (Rect p s@(Point w h)) = (p,p+pP w 0,p+pP 0 h,p+s)

lbrace = fst braces
rbrace = snd braces

braces = aboth bFlex2 (draw False, draw True)
  where
    draw right r = [DrawLines CoordModePrevious ls]
      where (tl,tr,bl,br) = corners (r `moverect` 1 `growrect` (-2))
            h = ycoord (bl-tl)
	    d = h `div` 2 - 4
            ls = if right
	         then [tl,east 1,se 2,south d,se 2,sw 2,south d,sw 2,west 1]
	         else [tr,west 1,sw 2,south d,sw 2,se 2,south d,se 2,east 1]

            west n = pP (-n) 0
	    east n = pP n 0
	    sw n = pP (-n) n
	    se n = pP n n
	    south n = pP 0 n

{-
polyLine p [] = []
polyLine p (v:vs) = (p,p'):polyLine p' vs
  where p' = p+v
-}

bFlex2 = bFlex' (pP 8 12)
bFlex = bFlex' (pP 5 10)
bFlex' size = FlexD size True False

rAngleBracket = bFlex2 (drawpoly . hMirror abPoints)
lAngleBracket = bFlex2 (drawpoly . abPoints)

abPoints = abPoints'
abPoints' r = [ur,ml,lr]
  where
    (ul@(Point lx ty),ur,_,lr@(Point rx by)) =
      corners (r `moverect` 1 `growrect` (-2))
    ml = Point (rx-d) my
    my = ty+h2
    h2 = (by-ty) `div` 2
    d = (h2 `div` 2) `min` (rx-lx)

triangleUp = flex' size (drawpoly . trianglePoints')
filledTriangleUp = flex' size (fillpoly . trianglePoints')
triangleDown = flex' size (drawpoly . vMirror trianglePoints')
filledTriangleDown = flex' size (fillpoly . vMirror trianglePoints')

size = Point 18 14

drawpoly ps = [DrawLines CoordModeOrigin ps]
fillpoly ps = [FillPolygon Convex CoordModeOrigin ps]
shrink = flip growrect (-1)

trianglePoints' = trianglePoints . shrink

trianglePoints (Rect p s@(Point w h)) = [p1,p2,p3,p1]
  where m = w `div` 2
	p1 = p+pP m 0
	p2 = p+s
	p3 = p+pP 0 h


vMirror f r@(Rect (Point x0 y0) s@(Point _ h)) =
    [ m p | p <- f (Rect (Point x0 0) s)]
  where m (Point x y) = Point x (y1-y)
        y1 = y0+h-1


hMirror f r@(Rect (Point x0 y0) s@(Point w _)) =
    [ m p | p <- f (Rect (Point 0 y0) s)]
  where m (Point x y) = Point (x1-x) y
        x1 = x0+w-1

padFD d (FlexD s fh fv f) = FlexD (s+diag (2*d)) fh fv f'
  where f' (Rect p s) = f (Rect (p+diag d) (s-diag d))
