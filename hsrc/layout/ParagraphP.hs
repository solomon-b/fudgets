module ParagraphP where
import LayoutRequest
import Geometry
--import ListUtil(chopList)
import HbcUtils(apFst,chopList)
import Defaults(defaultSep)
import Placers(verticalP')
import Spacers() -- synonym Distance, for hbc
import HorizontalAlignP(horizontalAlignP')
--import Maptrace(ctrace)
import Utils(oo)
import CmdLineEnv(argReadKey)
--import IntMemo

paragraphP = paragraphP' defaultSep
paragraphP' = paragraphP'' horizontalAlignP'

paragraphP'' :: (Int->Placer) -> Size -> Placer
paragraphP'' horizP' (Point hsep vsep) = P paP
  where
    paP reqs = ({-ctrace "paraReq" (req, wAdj req $ xcoord $ minsize req)-} req,paraPlacer2)
      where
	width0 = argReadKey "paragraph-width" 600 :: Int
	req = (paraReq width0) { wAdj=minsize . paraReq }
	paraReq width = fst (paraP width)
	paraPlacer2 rect@(Rect _ (Point w _)) =
	  --ctrace "paraPlacer2" rect $
	  snd (paraP w) rect
	paraP = {-memoInt-} paraP' -- memoInt slows down and consumes a lot of heap
	paraP' width = (vreq,placer2)
	  where
	    (vreq,vplacer2) = unP (verticalP' vsep) hreqs
	    placer2 = concat . zipWith id hplacers2 . vplacer2
	    (hreqs,hplacers2) =
	      unzip . map (unP (horizP' hsep)) . breakLines width $ reqs

	breakLines w rs = {-ctrace "breakLines" (w,map length rss)-} rss
	    where rss = breakLines' w rs

	breakLines' = chopList . (atLeastOne `oo` takeLine)
	takeLine wremain reqs = 
	  case reqs of
	    [] -> ([],[])
	    r:rs -> --ctrace "takeLine" (wremain,w) $
		    if wremain<w
		    then ([],reqs)
		    else apFst (r:) (takeLine (wremain-w-hsep) rs)
	      where w=xcoord (minsize r)

atLeastOne ([],x:xs) = ([x],xs)
atLeastOne xsys = xsys
