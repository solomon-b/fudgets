module Placers2(overlayP,
		verticalLeftP,verticalLeftP',
		horizontalCenterP,horizontalCenterP') where
import LayoutRequest
import Spacers(Distance(..))
import Geometry
import Defaults(defaultSep)
import Data.List(mapAccumL)
import IntMemo

overlayP :: Placer
overlayP = P overlayP'
  where
    overlayP' ls = (req,placer2)
      where
	ss = map minsize ls
	rps = concatMap refpoints ls
	wa w = Point w (maximum (1:map (ycoord . flip wAdj w) ls))
	ha h = Point (maximum (1:map (xcoord . flip hAdj h) ls)) h
	req = (refpLayout (pMax ss) (any fixedh ls) (any fixedv ls) rps) {wAdj=memoInt wa,hAdj=memoInt ha}
	placer2 r = [r | _ <- ls]

verticalLeftP = verticalLeftP' defaultSep

verticalLeftP' :: Distance -> Placer 
verticalLeftP' sep = P vlP
  where
    vlP ls = (req,placer2)
      where
	req = (refpLayout (Point w h) False (and fvs) (concat rpss)) {wAdj=memoInt wa}
	wa aw = Point aw (sum hs+totsep)
	  where hs = [ycoord (wAdj (min aw w)) | Layout{minsize=Point w _,wAdj=wAdj}<-ls]
	totsep = sep*(length ls-1)
	(ss,fhs,fvs) = unzipsfhv ls
	(ws,hs) = unzip [(w,h) | Point w h <- ss]
	w = maximum (1:ws)
	h = max 1 (h'-sep) -- (sum hs + sep*(length hs-1))
	(h',rpss) = mapAccumL adjust 0 ls
	  where adjust y (Layout {minsize=Point _ rh,refpoints=rps}) =
		    (y+rh+sep,map adj1 rps)
		  where adj1 (Point rx ry) = Point rx (y+ry)
	placer2 (Rect (Point x0 y0) _) = placer2' hs ss y0
	  where placer2' (h:hs) (s:ss) y =
		   Rect (Point x0 y) s:placer2' hs ss (y+sep+h)
		placer2' _ _ _ = []

horizontalCenterP = horizontalCenterP' defaultSep

horizontalCenterP' :: Distance -> Placer 
horizontalCenterP' sep = P hcP
  where
    hcP ls = (req,placer2)
      where
	req = (refpLayout (Point w h) (and fhs) False (concat rpss)) {hAdj=memoInt ha}
	(ss,fhs,fvs) = unzipsfhv ls
	ha ah = Point (sum ws+totsep) ah
	  where ws = [xcoord (hAdj (min ah h)) | Layout{minsize=Point _ h,hAdj=hAdj}<-ls]
	totsep = sep*(length ls-1)
	(ws,hs) = unzip [(w,h) | Point w h <- ss]
	w = max 1 w' --(sum ws + sep*(length ws-1))
	h = maximum (1:hs)
	(w',rpss) = mapAccumL adjust 0 ls
	  where adjust x (Layout {minsize=Point rw rh,refpoints=rps}) =
		    (x+rw+sep,map adj1 rps)
		  where adj1 (Point rx ry) = Point (x+rx) (ry+(h-rh) `div` 2)
	placer2 (Rect (Point x0 y0) (Point _ ah)) = placer2' ws hs ss x0
	  where placer2' (w:ws) (h:hs) (s:ss) x =
		  Rect (Point x (y0+(ah-h) `div` 2)) s:placer2' ws hs ss (x+sep+w)
		placer2' _ _ _ _ = []

unzipsfhv ls =
  unzip3 [(s,fh,fv) | Layout {minsize=s,fixedh=fh,fixedv=fv} <- ls]
