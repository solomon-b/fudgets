{-# LANGUAGE CPP #-}
module Edit(EditStop(..),editF, EditEvt(..), EditCmd(..)) where
import BgF
import Color
import Command
import DrawInWindow
import XDraw(clearArea)
import Defaults(inputFg, inputBg)
import CmdLineEnv(argReadKey, argKey)
import Dlayout(windowF)
import Edtypes
import Editfield
import Event
import Font
import Fudget
import FRequest
import Gc
import Geometry
import LayoutRequest(plainLayout,LayoutResponse(..))
import Message(message) --Message(..),
import NullF
import StateMonads
import Control.Monad(when)
import HbcUtils(apSnd)
import Xtypes
import UndoStack
import TryLayout
import Expose
import Maptrace
import GCAttrs(convFontK,fontdata2struct,FontSpec) -- instances
import InputMsg(InputMsg(InputChange))

default (Int) -- mostly for Hugs

data EditStop = 
     EditStopFn EditStopFn 
   | EditPoint Point 
   | EditLine EDirection

data EditCmd = 
     EditShowCursor Bool
   | EditMove EditStop IsSelect
   | EditReplace String
   | EditGetText
   | EditGetField
   | EditGetSelection
   | EditUndo
   | EditRedo 
     
data EditEvt
    = EditText String
    | EditField (String,String,String)
    | EditCursor Rect
    | EditChange (InputMsg String)
    deriving (Eq, Ord)

godir wanted current = if wanted < current then ELeft else ERight

toedstop :: (a ->String->String->(a,Maybe EDirection)) -> a -> EditStopFn
toedstop sf st b a = case sf st b a of
		      (_,Nothing) -> EdStop
		      (st',Just dir) -> EdGo dir (toedstop sf st')

notnull = not . null

inputbg = argKey "editbg" inputBg
inputfg = argKey "editfg" inputFg

selectbg = argKey "selectbg" inputfg
selectfg = argKey "selectfg" inputbg

editF :: FontSpec -> F EditCmd EditEvt
editF fontspec =
  let eventmask = [ExposureMask]
      startcmds = [XCmd $ ChangeWindowAttributes [CWBitGravity NorthWestGravity,
						  CWEventMask eventmask]]
  in  windowF startcmds (editK fontspec)

splitwith c [] = (([], False), [])
splitwith c (a : b) =
    if a == c
    then (([], True), b)
    else let ((x, g), y) = splitwith c b
	 in  ((a : x, g), y)

splitwithnl = splitwith newline
tabstop = 8
untab t s =
    case s of
      '\t':s -> let t' = (t `div` tabstop + 1) * tabstop
		in spaces (t'-t) ++ untab t' s
      c:s -> c:untab (if c == newline then 0 else (t+1)) s
      [] -> []

spaces n = replicate n ' '

editK fontspec = 
  convFontK fontspec $ \ fd ->
  fontdata2struct fd $ \ font ->
  changeGetBackPixel inputbg $ \bg -> 
  allocNamedColorPixel defaultColormap inputfg $ \fg -> 
  allocNamedColorPixel defaultColormap selectbg $ \sbg -> 
  allocNamedColorPixel defaultColormap selectfg $ \sfg -> 
  let fid = font_id font
      creategcs fg bg cont =
	wCreateGC rootGC [GCFunction GXcopy, GCFont fid,
			 GCForeground fg, GCBackground bg] $ \gc ->
	wCreateGC gc [GCForeground bg, GCBackground fg] $ \igc -> cont (gc,igc)
  in creategcs fg bg $ \drawGCs ->
     creategcs sfg sbg $ \selectGCs ->
     wCreateGC rootGC (invertColorGCattrs bg fg) $ \invertGC -> 
  let drawimagestring =
	if snd (font_range font) > '\xff'
	then wDrawImageString16
	else wDrawImageString
      getCurp = apSnd (eolx.fst) . getLnoEdge
      getLCurp = getCurp . setFieldDir ELeft
      getRCurp = getCurp . setFieldDir ERight
      npos = next_pos font
      eolx = npos . reverse . fst . splitnl
      maxrmargin x s =
	if null s
	then x
	else let (l,r) = splitnl s
	     in ctrace "editF1" (s,l,r,npos l) $ (x + npos l) `max` maxrmargin 0 r
      lno = fst
      xp = snd
      p2line (Point x y) = (y `quot` lheight, x - xoffset)
      line2p (l, x) = Point (x + xoffset) (l * lheight)
      lheight = linespace font
      move issel estop =
	do field <- loadField
	   lastpos <- loadLastpos
	   invIfShowCursor
	   let curp = getCurp field
	       stoppoint wantp p@(l, x) bef aft = 
		 let dircomp = godir wantp
		     dist p' = abs (lno wantp - lno p') + abs (xp wantp - xp p')
		     dir = dircomp p
		     ahead = if dir == ELeft then bef else aft
		 in case ahead of
		      [] -> (p, Nothing)
		      c:cs -> let p' = if c == newline
				       then (dirint dir + l, if dir == ERight
							     then 0
							     else eolx cs)
				       else (l, x + dirint dir * npos [c])
			      in  (p', if dir == dircomp p'
				       then Just dir
				       else if dist p' < dist p
					    then Just dir
					    else Nothing)
	       mf sf = moveField issel field sf
	       (field', acc) =
		 case estop of
		   EditStopFn stopf -> mf stopf
		   EditPoint p -> let lp = p2line p
				      dir = godir lp curp
				  in  mf (toedstop (stoppoint lp) curp)
		   EditLine dir -> let wantp = (dirint dir + lno curp, xp lastpos)
				   in  mf (toedstop (stoppoint wantp) curp)
	   storeField field'
	   let ol = lno curp
	       nl = lno $ getCurp field'
	   if issel
	      then showlines (min ol nl) (max ol nl)
	      else if notnull (getSelection field)
		   then showSelLines field >> showlines nl nl
		   else invIfShowCursor
      showSelLines field = 
	     showlines (lno $ getLCurp field) (lno $ getRCurp field) 
      setSize (l,x) =
	  do old@(ol,ox) <- loadTextWidth
	     let new@(_,x') = (l,max x minWidth)
	     when (old /= new) $
	       do storeTextWidth new
		  mtrace ("before trylayout "++show(x,x',ox))
		  x <- toMs $ tryLayoutK $
			 plainLayout (line2p (l,x') `padd` llmargin) True True
		  mtrace "after trylayout"
		  storeSize x
	    where mtrace x = toMsc (ctrace "editF" x)
      replace' s =
	  do field <- loadField
	     size <- loadSize
	     width <- loadWidth
	     let (ll,lx) = getLCurp field
		 uts = untab (length $ fst $ splitnl $ getBef field) s
		 rl = lno $ getRCurp field
		 field' = replaceField field uts
		 nls = nlines uts
		 nldown = nls - (rl - ll)
		 copy src dest h =
		   let srcp = line2p src
		       r = Rect srcp (pP width h)
		   in when (h>0) $
			putLowMs (wCopyArea (fst drawGCs) MyWindow
					    r (line2p dest))
	     (nlines,tw) <- loadTextWidth
	     let changemarg new f = maxrmargin lx (new ++ fst (splitnl (getAft f)))
		 oldm = changemarg (getSelection field) field
		 newm = changemarg uts field'
		 tw' = if newm >= tw
		       then newm
		       else if oldm < tw
			    then tw
			    else maxrmargin 0 (getField field')
		 ss = (getLastLineNo field' + 1, tw')
	     setSize ss
	     storeField' field'
	     us <- loadUndoStack
	     us' <- doit us (field',ss) return
	     storeUndoStack us'
	     when (nldown /= 0) $
		let tleft a = (rl + a + 1, 0)
		    tnl = tleft nldown
		in  copy (tleft 0) tnl (ycoord size - lheight * lno tnl)
	     showlines ll (ll + nls)
      dolines first last doline = du
	 where du s p@(l,x) = let ((line,nl), rest) = splitwithnl s
			      in if l > last || null s then return p
			      else when (l >= first) (doline p (line,nl)) >> 
				   du rest (if nl then (l+1,0) else (l,x+npos line))
      showLine (gc,rgc) lp (line, withnl) = 
	do let p = line2p lp
	       d = pP 0 (font_ascent font)
	   when (xp lp == 0) $
	     putLowMs (clearArea (rR 0 (ycoord p) xoffset lheight) False)
	   when (notnull line) $
	     putLowMs (drawimagestring gc (p+d) line)
	   when withnl $
	     do width <- loadWidth
		let pc = padd p (pP (npos line) 0)
		    size = Point (width - xcoord pc) lheight
		putLowMs (wFillRectangle rgc (Rect pc size))
      showlines first last =
	  do field <- loadField
	     showc <- loadShowCursor
	     let clno = lno $ getLCurp field
		 sel = getSelection field
		 aft = getAft field
		 takenl n s = let (l,r) = splitnl s
			      in if n <= 0 then l else l++newline:takenl (n-1) r
		 bef = reverse $ takenl (clno-first) $ getBef field
		 show gcs = dolines first last (showLine gcs)
	     show drawGCs bef (clno-nlines bef,0) >>=
		show (if showc then selectGCs else drawGCs) sel >>=
		show drawGCs (aft++[newline]) >>= \_ ->
		when (clno >= first && clno <= last) invIfShowCursor
      showCursor v = do cv <- loadShowCursor
			when (v /= cv ) $
			  do field <- loadField
			     storeShowCursor v
			     if null (getSelection field) then
			       invCursor
			       else showSelLines field
      invIfShowCursor = do cv <- loadShowCursor
			   when cv invCursor
      invCursor = do field <- loadField
		     let lp = getCurp field
			 sel = getSelection field
		     when (null sel) $
		       let p = line2p (apSnd ((-1) +) lp)
			   s = pP 1 lheight
			   cur = Rect p s
		       in  putLowMs (wFillRectangle invertGC cur)
      redraw = do --field <- loadField
		  size <- loadSize
		  putLowMs (clearArea (Rect origin size) True)
      expose r = let Line l1 l2 = rect2line r
		 in showlines (lno (p2line l1)) (lno (p2line l2) + 1)
      undoredo d =
	do us <- loadUndoStack
	   case d us of
	     Nothing -> nopMs
	     Just ((field,size),us') -> do storeUndoStack us'
					   storeField' field
					   setSize size
					   redraw

      storeField' field' =
	do storeField field'
	   putHighMs (EditChange $ InputChange $ getField field')

      puttext' f = do field <- loadField
		      putHighMs (f field)

      puttext f = puttext' (EditText . f)

      putCursor =
        do field <- loadField
	   let lastpos = getCurp field
	   putHighMs (EditCursor $ Rect (line2p lastpos `psub` Point xoffset 0) 
					(Point xoffset lheight `padd` llmargin))

      handleLow msg =
	case msg of
	  XEvt (Expose r aft) -> toMs (maxExposeK False r aft) >>= expose
	  XEvt (GraphicsExpose r aft _ _) -> toMs (maxExposeK True r aft) >>= expose
	  LEvt (LayoutSize s) -> storeSize s
          _ -> nopMs

      handleHigh cmd =
	do case cmd of
	     EditShowCursor s -> showCursor s
	     EditMove estop issel -> move issel estop
	     EditReplace s -> replace' s
	     EditGetText -> puttext getField
	     EditGetField -> puttext' (EditField . getField')
	     EditGetSelection -> puttext getSelection
	     EditUndo -> undoredo undo
	     EditRedo -> undoredo redo
	   putCursor
	   field <- loadField
	   let lastpos = getCurp field
	   case cmd of
	     EditMove (EditLine _) _ -> nopMs
	     _ -> storeLastpos lastpos

      proc = do message handleLow handleHigh =<< getKs
		proc


  in  stateK initstate (setSize (1,0) >> proc) nullK

minWidth = 10
xoffset = 2
llmargin = Point 2 2

defaultuslimit = Nothing
uslimit = let ul = argReadKey "undodepth" (-1)
	  in if ul == -1 then defaultuslimit else Just ul

data EditState a = S { shocur :: Bool,
		       twidth :: (Int,Int),
		       undostack :: UndoStack a,
		       field :: EditField,
		       size :: Point,
		       lastpos :: (Int,Int)
		       }

--initstate = (False,(1,0),undoStack uslimit, createField "", origin, (0, 0))
initstate = S False (1,0) (undoStack uslimit) (createField "") origin (0, 0)

loadShowCursor = fieldMs shocur
loadTextWidth = fieldMs  twidth
loadUndoStack = fieldMs  undostack
loadField = fieldMs field
loadSize = fieldMs size
loadLastpos = fieldMs lastpos
--loadWidth = loadSize >>= \size -> return (xcoord size)
loadWidth = fmap xcoord loadSize


#define MODMS(lbl) ( \ lbl -> (modMs ( \ s -> s { lbl=lbl } )))

storeShowCursor = MODMS(shocur)
storeTextWidth  = MODMS(twidth)
storeUndoStack  = MODMS(undostack)
storeField      = MODMS(field)
storeSize       = MODMS(size)
storeLastpos    = MODMS(lastpos)
