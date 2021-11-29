module Rects where
import Geometry

--newtype Region = Region [Rect]

intersectRects rs (Rect p2 s2) = ir rs
  where
    br2 = p2+s2
    ir [] = []
    ir (Rect p1 s1:es) =
      let ul = pmax p1 p2
          br = pmin (p1+s1) br2
	  s@(Point w h) = br-ul
      in if w<=0 || h<=0 then ir es else Rect ul s:ir es

overlaps (Rect (Point x1 y1) (Point w1 h1))
         (Rect (Point x2 y2) (Point w2 h2)) =
  x1<x2+w2 && x2<x1+w1 && y1<y2+h2 && y2<y1+h1
--x1<=x2+w2 && x2<=x1+w1 && y1<=y2+h2 && y2<=y1+h1
-- rR 0 0 10 10 doesn't overlap with rR 0 10 10 10

boundingRect r1@(Rect p1 s1) r2@(Rect p2 s2) =
    if s1==0 then r2 else if s2==0 then r1 else Rect p s
  where p = pmin p1 p2
        s = pmax (p1+s1) (p2+s2) - p

diffRect r1 r2@(Rect (Point x2 y2) (Point w2 h2)) =
    if overlaps r1 r2 -- faster handling of common(?) case
    then intersectRects outside_r2 r1
    else [r1]
  where
    u@(Rect p1@(Point x1 y1) s1@(Point w1 h1)) = boundingRect r1 r2
    outside_r2 = [a,b,c,d]
    a = Rect p1 (Point w1 (y2-y1))
    b = Rect (Point x1 y2) (Point (x2-x1) h2)
    c = Rect (Point xc y2) (Point (x1+w1-xc) h2) where xc = x2 + w2
    d = Rect (Point x1 yd) (Point w1 (y1+h1-yd)) where yd = y2+h2

{-
    u:
       +-----------------------------+
       |            a                |
       |                             |
       +--------+-------+------------+
       |   b    |  r2   |     c      |
       |        |       |            |
       +--------+-------+------------+
       |                             |
       |             d               |
       |                             |
       +-----------------------------+
-}
