module UserCoords where
import Fudgets(Point(..))

type Interval =(Double,Double)
type Area = (Interval,Interval)

wdw h (u1,u2) u = ftoi' (0.5+(u-u1)/(u2-u1)*real h)
user w (u1,u2) u = (real u-0.5)/real w*(u2-u1)+u1

userp ((Point w h),(xa,ya)) (Point x y) = (user w xa x,user h ya y)
wdwp ((Point w h),(xa,ya)) (x,y) = Point (wdw w xa x) (wdw h ya y)

zoom k (Point w h) (Point x y) (xa,ya) = (zoom2 k w x xa,zoom2 k h y ya)
  where
    zoom2 k tot rel ua@(u1,u2) = (zoom1 k u1 u0,zoom1 k u2 u0)
      where
        u0 = user tot ua rel
	zoom1 k u u0 = k*(u-u0)+u0

zoomin = zoom 0.5
zoomout = zoom 2.0

zoomrect sa p1 p2 =
  let (nx1,ny1) = userp sa p1
      (nx2,ny2) = userp sa p2
  in ((nx1,nx2),(ny1,ny2))

--

real :: Int->Double
real = fromIntegral

ftoi :: Double->Int
ftoi = floor

ftoi' x =     let maxint = 32767 -- Xlib limitation
	          max = real maxint
	          negmax = 0.0-max
	      in if x>max then maxint
		 else if x<negmax then -maxint
		 else ftoi x
