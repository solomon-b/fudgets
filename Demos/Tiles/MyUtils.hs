module MyUtils where

import AllFudgets

mapstate f os [] = (os, [])
mapstate f os (h:t) = let (ns, el) = f os h in
			let (nns, ell) = mapstate f ns t in
				(nns, el:ell)

throRight :: F a b -> F (Either a c) (Either b c)
throRight = idRightF

line x1 y1 x2 y2 = [DrawLine (Line (Point x1 y1) (Point x2 y2))]

circle x y r =
	[FillArc (Rect (psub (Point x y) (Point r r)) (Point (2*r) (2*r))) 0 (64*360)]
