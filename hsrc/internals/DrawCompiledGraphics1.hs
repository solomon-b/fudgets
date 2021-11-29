module DrawCompiledGraphics1(drawK',drawChangesK',GCId) where
--import Fudget
import Xtypes
import XDraw(DrawCommand(FillRectangle),clearArea,draw,drawMany,Drawable(..))
import Geometry(growrect,(=.>),Rect(rectsize))
--import Message
--import NullF(putsK,putK)
import Utils(number)
--import EitherUtils(mapfilter)
import Data.Maybe(mapMaybe)
import CompiledGraphics
--import Rects
--import Maptrace(ctrace) -- debug
--import Io(echoK) -- debug
import FudgetIO(putLow)

--tr x = seq x $ ctrace "drawtrace" x x
--trLow = Low . tr
--trLow = tr . Low
--maptrLow = map trLow
--debugK = echoK

--drawK = drawK' MyWindow
drawK' d (higc,hiR) clip cg =
    case draw cg [] of
      [] -> id
      cmds -> putLow $ drawMany d cmds
  where
    draw (CGMark cg) = draw cg
    draw (CGraphics r cur cmds gs) =
      (if cur
       then ((higc,[FillRectangle cr | hr<-hiR r,cr<-clip hr]):)
       else id).
      (cmds++) .
      draws gs
    draws [] = id
    draws (g:gs) = draw g . draws gs

drawChangesK' d beQuick higc  (CGMark cg) (CGMark ocg) changes =
    --debugK (show [ ps | ps<-changes, take 1 ps/=[0]]) .
    drawChangesK' d beQuick higc cg ocg (mapMaybe drop0 changes)
  where drop0 [] = Just []
        drop0 (0:ps) = Just ps
	drop0 _ = Nothing

drawChangesK' d beQuick higc  cg@(CGraphics r  _ cmds cgs )
                          ocg@(CGraphics or _ ocmds ocgs) changes =
    --debugK (unwords ["Changes:",show changes,"or",show or,"nr",show r]) .
    if r/=or || [] `elem` changes
       -- Hack for overlapping parts:
       || not (null changes || null cmds && null ocmds)
    then --debugK "Drawing" .
         -- !! test if scrolling is enough
	 eraseOldK d r or .
         reDrawK' d beQuick higc cg
    else if null changes
         then --debugK "Pruning" .
	      id
	 else --debugK "Descending" .
	      let changes' i= [ p | i':p <- changes, i'==i]
	      in foldr (.) id [drawChangesK' d beQuick higc cg ocg (changes' i) |
				(i,(cg,ocg))<-number 1 (zip cgs ocgs)]
drawChangesK' d beQuick higc  cg ogc _ =
    --debugK "drawNewK" .
    drawNewK cg
  where
    drawNewK (CGMark cg) = drawNewK cg
    drawNewK cg@(CGraphics r _ _ _) =
      eraseOldK d r (cgrect ogc) .
      reDrawK' d beQuick higc cg

eraseOldK Nothing newrect oldrect =
  -- It's enough to clear the part of oldrect that is outside newrect.
  ifK (newrect/=oldrect)
      (putLow $ clearArea (growrect oldrect 1) False)
eraseOldK (Just (d,cleargc)) newrect oldrect =
  ifK (newrect/=oldrect)
      (putLow $ draw d cleargc (FillRectangle (growrect oldrect 1)))

reDrawK' d beQuick higc (CGMark cg) = reDrawK' d beQuick higc cg
reDrawK' Nothing beQuick higc cg@(CGraphics r _ _ _) =
  -- When drawing directly in a window:
  if (not beQuick || rectsize r =.> 400) -- heuristic
  then -- for big areas: wait for exposure event and draw only the
       -- visible part
       putLow (clearArea r True)
  else -- for small areas: draw everything immediately (reduced flicker)
       putLow (clearArea r False) . drawK' MyWindow higc (:[]) cg
reDrawK' (Just (d,cleargc)) beQuick higc cg@(CGraphics r _ _ _) =
       -- For drawing in a back buffer or a pixmap (assumes d/=MyWindow):
       putLow (draw d cleargc (FillRectangle r)) .
       drawK' d higc (:[]) cg

ifK b k = if b then k else id
