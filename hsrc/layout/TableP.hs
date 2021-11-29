{-# LANGUAGE CPP #-}
module TableP(tableP,tableP') where
import Geometry(Point(..), Rect(..), rR, xcoord, ycoord)
import LayoutDir(LayoutDir(..), vswap)
import LayoutRequest
import Spacers(Distance(..))
import HbcUtils(chopList)
import Data.List(transpose,mapAccumL)
import Utils(lhead)
import Defaults(defaultSep)
import IntMemo

import Maptrace(ctrace)
tr x = ctrace "tableP" x x

tableP n = tableP' n Horizontal defaultSep

tableP' :: Int -> LayoutDir -> Distance -> Placer
tableP' count' ld sep = P $ \ requests ->
    let --sizes = map minsize requests
        (rows, columns) =
            let hmatrix = chopList (splitAt count') requests
                vmatrix = transpose hmatrix
            in  vswap ld (hmatrix, vmatrix)
        nrows = length rows
        ncols = length columns
        vsep = (nrows - 1) * sep
        hsep = (ncols - 1) * sep
        rowhs = map (maximum . (0:) . map (ycoord.minsize)) rows
        colws = map (maximum . (0:) . map (xcoord.minsize)) columns
	--rowfixws = map (and . map fixedh) rows
	rowfixhs = map (or . map fixedv) rows
	colfixws = map (or . map fixedh) columns
	--colfixhs = map (and . map fixedv) columns
        h = sum rowhs
        w = sum colws
        toth = h + vsep
        totw = w + hsep
	tot = Point totw toth
	totfh = and rowfixhs
	totfw = and colfixws
        rps = concatMap (\(r,p)->map (p+) (refpoints r)) (zip requests cellps)
	  where	cellps = [Point x y | y<-place 0 sep rowhs,x<-place 0 sep colws]
--	  where	cellps = [Point x y | y<-0:init rowhs,x<-0:init colws] --sep??
        table2 (Rect (Point x0 y0) got@(Point width height)) =
            let --Point extraw extrah = (got `psub` tot) --`pmax` origin
#if 0
-- old solution
		rowhs' = arowhs height --pad flexh extrah rowhs rowfixhs
		colws' = acolws width --pad flexw extraw colws colfixws
#else
-- new solution
		((colws',rowhs'),(w',h')) =
		  if width<=totw -- hmm...
		  then (adjrowhs width,(width,sum rowhs'+vsep))
		  else (adjcolws height,(sum colws'+hsep,height))
#endif
		colws'' = adjsizes (tr (width-w')) colws' colfixws
		rowhs'' = adjsizes (tr (height-h')) rowhs' rowfixhs
		xs = place x0 sep colws''
		ys = place y0 sep rowhs''
		placedrows =
		  [[rR x y w h|(x,w)<-zip xs colws'']|(y,h)<-zip ys rowhs'']
{- old
		w' = sum colws'
		h' = sum rowhs'
		hscale,vscale :: Double
                hscale = fromInt (width - hsep) / fromInt w'
                vscale = fromInt (height - vsep) / fromInt h'
                placecols x y h' [] = []
                placecols x y h' (w' : ws) =
                    let w'' = scale hscale w'
                    in  rR x y w'' h' : placecols (x + w'' + sep) y h' ws
                placerows y [] = []
                placerows y (h' : hs) =
                    let h'' = scale vscale h'
                    in  placecols x0 y h'' colws' : placerows (y + h'' + sep) hs
                placedrows = placerows y0 rowhs'
-}

                rectss =
                    case ld of
                      Horizontal -> placedrows
                      Vertical -> transpose placedrows
		rects = concat rectss
            in (if length rects<length requests 
	        then ctrace "tableP" (length requests,length rects)
	        else id) $
	       lhead requests rects

	acolws aw = adjsizes (aw-totw) colws colfixws
	arowhs ah = adjsizes (ah-toth) rowhs rowfixhs

	adjsizes extra ss fixs = pad flex extra ss fixs
	  where
	    flex = sum [s | (s,fixed) <-zip ss fixs, not fixed]

	    pad _    0     ws _ = ws
	    pad 0    extra ws _ = ws
	    pad flex extra (w:ws) (fixed:fs) =
	      if fixed
	      then w:pad flex extra ws fs
	      else let e = (extra*w `quot` flex) `max` (-w)
		   in w+e:pad (flex-w) (extra-e) ws fs
	    pad _ _ _ _ = []

	adjrowhs = memoInt adjrowhs'
	adjrowhs' w = (colws,map maximum (transpose colhs))
	  where colhs = [map (ycoord . flip wAdj colw) column |
			  (colw,column) <- zip colws columns]
	        colws = acolws w

	adjcolws = memoInt adjcolws'
	adjcolws' h = (map maximum (transpose rowws),rowhs)
	  where rowws = [map (xcoord . flip hAdj rowh) row |
			  (rowh,row) <- zip rowhs rows]
		rowhs = arowhs h

	wa w = ctrace "tablePwa" s s
	  where s = Point w (vsep+h)
	        h = sum (snd (adjrowhs w))
{- --old:
		h = sum . map maximum . transpose $ colhs
		colhs =  [map (ycoord . flip wAdj colw) col |
			  (colw,col) <- zip (acolws w) columns]
-}
	ha h = Point (hsep+w) h
	  where w = sum (fst (adjcolws h))
{- --old:
		w = sum . map maximum . transpose $ rowws
		rowws =  [map (xcoord . flip hAdj rowh) row |
			  (rowh,row) <- zip (arowhs h) rows]
-}
    in ((refpLayout tot totfw totfh rps){wAdj=memoInt wa,hAdj=memoInt ha}, table2)

place pos0 sep = snd . mapAccumL f pos0
  where f pos size = (pos+size+sep,pos)

#ifdef __NHC__
fromInt = fromIntegral
#endif
