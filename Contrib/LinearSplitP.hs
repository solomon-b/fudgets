module LinearSplitP where
import AllFudgets
--import ListUtil(chopList)
import HbcUtils(apFst,chopList)
import Data.Maybe(isJust,listToMaybe)

horizontalSplitP = horizontalSplitP' defaultSep
verticalSplitP = verticalSplitP' defaultSep

horizontalSplitP' = linearSplitP Horizontal
verticalSplitP' = linearSplitP Vertical

linearSplitP dir sep = P linearSplitP'
  where
    linearSplitP' [] = linearP' []
    linearSplitP' [r] = linearP' [r]
    linearSplitP' reqs0 = (req,placer2)
      where
        reqss = chopReqs reqs0
        (reqs1,placers2) = unzip (fmap linearP' reqss)
	--reqs2 = zipWith adjSize (sizes reqss) reqs1
	(req,placer2a) = linearP' reqs1
	positions = fmap ( \ r->listToMaybe r >>= wantedPos) reqss
	placer2 r@(Rect _ s) =
 	  concat . zipWith id placers2 . adjPlaces s positions . placer2a $ r

    adjPlaces asize (_:ps) (r:rs) = adjPlaces' ps r rs
      where
	adjPlaces' (optp:ps) r1@(Rect p1 s1) (r2@(Rect p2 s2):rs) =
	  case optp of
	    Nothing -> r1:adjPlaces' ps r2 rs -- shouldn't happen
	    Just (p0,s,a) -> r1' : adjPlaces' ps r2' rs
	      where v = mkp dir d 0
	             where
		       d = max 1 (d0+d1)-d1 -- try to avoid sizes <= 0
		       d0 = xc dir p-xc dir (rectpos r2)
		       d1 = xc dir s1
		    p = p0 + scalePoint a (asize-s)
		    r1' = Rect p1 (s1+v)
		    r2' = Rect (p2+v) (s2-v)
	adjPlaces' [] r [] = [r]


    chopReqs = chopList splitReqs

    splitReqs (r:rs) = apFst (r:) (break wantPos rs)
    splitReqs [] = ([],[])

    wantPos = isJust . wantedPos

    linearP' = unP (linearP dir sep)

    {-
    adjSize Nothing req = req
    adjSize (Just s1) req@(Layout{minsize=s2}) =
        req{minsize=size, wAdj=const size, hAdj=const size}
      where size = mkp dir (xc dir s1) (yc dir s2)

    sizes = sizes' . (Just 0:) . tail . positions
    sizes' ps = zipWith size ps (tail ps++[Nothing])
      where size optp1 optp2 = do p1 <- optp1
                                  p2 <- optp2
				  return (p2-p1-mkp dir sep 0)
    -}
