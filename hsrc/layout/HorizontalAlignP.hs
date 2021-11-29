module HorizontalAlignP where
import Data.List(mapAccumL)
import LayoutRequest
import Geometry
import Spacers(Distance(..),layoutModifierS,idS)
import Defaults(defaultSep)

-- Better names:
alignP = horizontalAlignP
alignP' = horizontalAlignP'

horizontalAlignP = horizontalAlignP' defaultSep

horizontalAlignP' :: Distance -> Placer
horizontalAlignP' sep = P haP
  where
    haP reqs = (req,placer2)
      where
	sepp = pP sep 0
	req = refpLayout rsize fh fv [ref1,ref2]
	fh = all fixedh reqs
	fv = False -- any fixedv reqs
	reqrects0 = snd $ mapAccumL reqrect (-sepp) reqs
	  where
	    reqrect ref0 (Layout {minsize=s,fixedh=fh,fixedv=fv,refpoints=rps}) =
		(ref2',((Rect d s,(fh,fv)),(d+ref1,ref2')))
	      where d = ref0-ref1+sepp
		    ref2' = d+ref2
		    (ref1,ref2) = case rps of
				    [] -> middleRefs s
				    _  -> (head rps,last rps)

	reqrects = map adj reqrects0
	  where adj ((r,f),(ref1,ref2)) =
		  ((moverect r (-minp),f),(ref1-minp,ref2-minp))
		minp = pMin (0:[d | ((Rect d _,_),_) <- reqrects0])

	rsize = pMax (1:[p+s | ((Rect p s,_),_) <- reqrects])
	(ref1,ref2) =
	  case reqrects of
	    [] -> middleRefs rsize
	    _ -> (fst . snd . head $ reqrects,snd . snd . last $ reqrects)

	placer2 rect@(Rect p asize) = [moverect r d | ((r,_),_)<-reqrects]
	  where d = p -- + scalePoint 0.5 (pmax 0 (asize-rsize))

--refMiddleS :: Spacer
refMiddleS = S refMiddleS'

refMiddleS' req =
 let (ref1,ref2) = middleRefs (minsize req)
 in (req{refpoints=[ref1,ref2]},id)
-- in (Layout s fh fv wa ha [ref1,ref2] wanted,id)

--refEdgesS :: Spacer
refEdgesS = S refEdgesS'
  where
    refEdgesS' req@(Layout {refpoints=[]}) = refMiddleS' req
    refEdgesS' req@(Layout {minsize=Point w _,refpoints=rps}) =
        (req {refpoints=[ref1,ref2]},id)
      where
        ref1 = (head rps){xcoord=0}
	ref2 = (last rps){xcoord=w}

middleRefs (Point w h) = (pP 0 h2,pP w h2)
  where h2 = h `div` 2

noRefsS :: Spacer
noRefsS = S $ \ req -> (req {refpoints=[]},id)

moveRefsS :: Point -> Spacer
moveRefsS d = layoutModifierS (mapLayoutRefs (d+))

---

spacersP :: Placer -> [Spacer] -> Placer
spacersP (P placer) spacers = P $ \ reqs ->
  let
    (reqs',spacers2) = unzip (zipWith unS (spacers++repeat idS) reqs)
    (req,placer2) = placer reqs'
    placer2' = zipWith id spacers2 . placer2
  in  (req,placer2')

---
overlayAlignP :: Placer
overlayAlignP = P $ \ ls ->
  let maxrp = maximum [head (refpoints l) | l<-ls, not (null (refpoints l))]
      ss = [f ms rps | Layout { minsize=ms,refpoints=rps}<-ls ]
	where f s [] = s
	      f s (rp:_) = s+maxrp-rp
      req = refpLayout (pMax ss) (any fixedh ls) (any fixedv ls) [maxrp]
      placer2 r = [f r rps | Layout {refpoints=rps} <- ls]
	where f r [] = r
	      f (Rect p s) (rp:_) = Rect (p+d) (s-d)
		where d=maxrp-rp
  in (req,placer2)
