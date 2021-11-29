{-# LANGUAGE CPP #-}
module Geometry where
-- This module should be moved to ../types/

import Data.Ix

class Move a where move :: Point -> a -> a

fmove 0 = id
fmove p = fmap (move p)

instance Move a => Move [a]       where move = fmove
instance Move a => Move (Maybe a) where move = fmove

data Point = Point { xcoord, ycoord :: Int }
   deriving (Eq, Ord, Show, Read, Ix)
type Size = Point
data Line = Line Point Point  deriving (Eq, Ord, Show, Read)
data Rect = Rect {rectpos::Point, rectsize::Size}
 deriving (Eq, Ord, Show, Read)
{-
instance Show Point where showsPrec d (Point x y) = showsPrec d (x,y)
instance Read Point where readsPrec d s = [(pP x y,r)|((x,y),r)<-readsPrec d s]

instance Show Rect where
  showsPrec d (Rect p s) =
    showParen (d>=10) $
    showString "R " . showsPrec 10 p . showChar ' ' . showsPrec 10 s
-}
-- convenient abbreviations:
origin = Point 0 0
pP x y = Point x y
lL x1 y1 x2 y2 = Line (Point x1 y1) (Point x2 y2)
rR x y w h = Rect (Point x y) (Point w h)
diag x = Point x x

-- selectors:
--xcoord (Point x _) = x
--ycoord (Point _ y) = y

--rectsize (Rect _ size) = size
--rectpos (Rect pos _) = pos

-- basic operations:
padd (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
psub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

instance Num Point where
	 (+) = padd
	 (-) = psub
	 (*) = error "(*) on Point"
	 negate = psub origin
--	 abs = error "abs on Point"
--	 signum = error "signum on Point"
	 fromInteger i = let i' = fromInteger i in Point i' i'
#ifdef __HBC__
	 fromInt i = Point i i
#endif

rsub (Rect p1 _) (Rect p2 _) = psub p1 p2

posrect (Rect pos size) newpos = Rect newpos size
moverect (Rect pos size) delta = Rect (padd pos delta) size
sizerect (Rect pos size) newsize = Rect pos newsize
growrect (Rect pos size) delta = Rect pos (padd size delta)

moveline (Line p1 p2) delta = Line (padd p1 delta) (padd p2 delta)

rect2line (Rect p s) = Line p (p `padd` s)
line2rect (Line p1 p2) = Rect p1 (p2 `psub` p1)

instance Move Point where move = padd
instance Move Rect where move = flip moverect
instance Move Line where move = flip moveline

-- misc:
Point x1 y1 =.> Point x2 y2 = x1 >= x2 && y1 >= y2
inRect pt (Rect p1 p2) = pt =.> p1 && p2 =.> psub pt p1
scale k i = truncate (k * fromIntegral i)
scalePoint k (Point x y) = Point (scale k x) (scale k y)
rectMiddle (Rect (Point x y) (Point w h)) =
    Point (x + w `quot` 2) (y + h `quot` 2)

freedom (Rect _ outer) (Rect _ inner) = psub outer inner

pmin (Point x1 y1) (Point x2 y2) = Point (x1 `min` x2) (y1 `min` y2)
pmax (Point x1 y1) (Point x2 y2) = Point (x1 `max` x2) (y1 `max` y2)

pMin (p : pl) = foldr pmin p pl
pMin [] = error "pMin on []"

pMax (p : pl) = foldr pmax p pl
pMax [] = error "pMax on []"

plim p0 p1 p = pmax p0 (pmin p1 p)

-- | confine outer inner: moves an shrinks inner to fit within outer
confine (Rect outerpos outersize) (Rect innerpos innersize) =
    let newsize = pmin outersize innersize
        maxpos = padd outerpos (psub outersize newsize)
    in  Rect (plim outerpos maxpos innerpos) newsize

-- | rmax gives an enclosing rect
rmax r1 r2 = line2rect (Line (pmin lp1 lp2) (pmax lp1' lp2'))
   where Line lp1 lp1' = rect2line r1
	 Line lp2 lp2' = rect2line r2
