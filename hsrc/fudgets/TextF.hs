{-# LANGUAGE CPP #-}
module TextF(textF,textF',textF'',TextF,
	     TextRequest(..)) where
import Fudget
import FudgetIO
import FRequest
import NullF
import Utils
import Geometry
import Xtypes
import Event
import Command
import XDraw
import Dlayout
import DoubleClickF
import BgF
--import Color
--import EitherUtils(mapfilter)
import Data.Maybe(mapMaybe)
import Message(message) --Message(..),
import Font
--import LoadFont
import Gc
import InputMsg
import LayoutRequest
import Alignment(aLeft) --Alignment(..),
import Defaults(defaultFont,bgColor,fgColor)
import Sizing
import FDefaults
import GCAttrs --(ColorSpec,convColorK,colorSpec)
import ListRequest(ListRequest(..),listEnd)
#include "../defaults/defaults.h"

default(Int) -- mostly for Hugs


#ifndef __HBC__
#define fromInt fromIntegral
#endif

type TextRequest = ListRequest String

newtype TextF = Pars [Pars]

data Pars
  = BorderWidth Int
  | FgColorSpec ColorSpec
  | BgColorSpec ColorSpec
  | FontSpec FontSpec
  | Align Alignment
  | Margin Int
  | InitText [String]
--  | InitSize String
  | Stretchable (Bool,Bool)
  | Sizing Sizing

parameter_instance(BorderWidth,TextF)
parameter_instance(FgColorSpec,TextF)
parameter_instance(BgColorSpec,TextF)
parameter_instance(FontSpec,TextF)
parameter_instance(Align,TextF)
parameter_instance(Margin,TextF)
parameter_instance(InitText,TextF)
--parameter_instance(InitSize,TextF)
parameter_instance(Sizing,TextF)
parameter_instance(Stretchable,TextF)

textF = textF' standard
textF' pm = noPF $ textF'' pm

textF'' :: Customiser TextF ->
           PF TextF TextRequest (InputMsg (Int, String))
textF'' pmod =
  let ps :: TextF
      ps = pmod (Pars [BorderWidth 0,
                       FgColorSpec textfg,
		       BgColorSpec textbg,
		       Margin 2,
		       Align aLeft,
		       InitText [],--InitSize "",
		       Stretchable (False,False),
		       Sizing Dynamic,
		       FontSpec (fontSpec defaultFont)])
      bw = getBorderWidth ps
      fg = getFgColorSpec ps
      bg = getBgColorSpec ps
      font = getFontSpec ps
      init = getInitText ps
      minstr = "" --getInitSize ps
      margin = getMargin ps
      align = getAlign ps
      sizing = getSizing ps
      stretch = getStretchable ps

      eventmask = [ExposureMask, ButtonPressMask]
      startcmds = map XCmd 
                  [ConfigureWindow [CWBorderWidth bw],
  		   ChangeWindowAttributes
		     [CWEventMask eventmask
		      ,CWBitGravity (horizAlignGravity align)
		      ,CWBackPixmap none -- elim flicker caused by XClearArea
		      ]]
  in doubleClickF doubleClickTime $
     windowF startcmds $ textK0 bg fg font stretch align sizing margin minstr init


textK0 bg fg font (flexh,flexv) align sizing margin minstr init =
    changeGetBackPixel bg $ \ bgcol ->
    convColorK fg $ \ fgcol ->
    --allocNamedColorPixel defaultColormap fg $ \ fgcol ->
    convFontK font $ \ fd ->
    fontdata2struct fd $ \ fs ->
    wCreateGC rootGC [GCFont (font_id fs),
  		      GCForeground fgcol,
		      GCBackground bgcol] $ \gc ->
    wCreateGC gc     [GCForeground bgcol,
		      GCBackground fgcol] $ \gcinv ->
    let minw = next_pos fs minstr
    in textK1 bgcol gc gcinv fs (not flexh) (not flexv) align sizing margin minw init

textK1 bgcol gc gcinv fs fh fv align sizing margin minw =
    replaceTextK origin origin [] [] 0 listEnd
  where
    ll size = Low (layoutRequestCmd (plainLayout size fh fv))
    ls = linespace fs
    base = font_ascent fs + margin
    margsize = diag (2*margin)

    measure = map (pairwith (next_pos fs))
    txtwidth mtxt = maximum (1:minw:map snd mtxt)
                         -- 0 width not allowed for windows

    drimstr = if snd (font_range fs) > '\xff'
              then DrawImageString16
	      else DrawImageString

    txtsize mtxt =
      let width = txtwidth mtxt
	  height = max 1 (ls*length mtxt)  -- 0 height not allowed for windows
      in Point width height

    replaceTextK winsize@(Point winwidth winheight) size sel mtxt dfrom dcnt newtxt=
      let lines     = length mtxt
	  from      = min lines (if dfrom==listEnd then lines else dfrom)
	  after     = lines-from
	  cnt       = min after (if dcnt==listEnd then after else dcnt)
	  newcnt    = length newtxt
	  diff      = newcnt-cnt
	  scrollsize= after-cnt
	  newlines  = lines+diff
	  sel'      = mapMaybe reloc sel
	  reloc n   = if n<from then Just n
		      else if n<from+cnt then Nothing
		      else Just (n+diff)
	  mtxt'     = take from mtxt ++ measure newtxt ++ 
		      (if scrollsize>0 then drop (from+cnt) mtxt else [])
	  newwidth  = txtwidth mtxt'
	  newsize   = Point newwidth (ls*newlines)
	  llcmd     = let realwinsize@(Point w h) = winsize+diag margin
	                  winsize'@(Point w' h') = newsize +margsize
	                  change =
			    winsize==origin ||
			    newSize sizing realwinsize winsize'/=realwinsize
	              in if change
		      then [ll (newsize + margsize)]
		      else []
	  --width     = xcoord size
	  drawwidth = max newwidth (winwidth-margin)
		       -- !! always scrolls/clears the full width of the window
	  scrollrect= rR margin (margin+ls*(from+cnt))
	                 drawwidth (ls*scrollsize)
	  scrolldest= Point margin (margin+ls*(from+newcnt))
	  scrollcmd = if scrollsize>0 && diff/=0
		      then [Low (wDraw gc $ CopyArea MyWindow scrollrect scrolldest)]
		      else []
	  drawrect  = rR margin (margin+ls*from) (drawwidth+margin) (ls*newcnt)
	                 -- add margin to width to erase text in the margin
			 -- when the text is wider than the window.
	  belowrect = rR margin (margin+ls*newlines) drawwidth (-ls*diff)
	  clearcmd  = (if newcnt>0
		       then let vrect = growrect drawrect (pP 5 5) -- !! tmp fix
		           in clearArea drawrect True++
		              [Low (LCmd (layoutMakeVisible vrect))]
		       else [])++
		       (if diff<0
		        then [Low $ XCmd $ ClearArea belowrect False]
			     -- Needed because of margin and other things
			     -- that cause the window to be taller than the
			     -- text.
			     -- clearcmd must be done after scrollcmd !!
			else [])
	  clearArea r e = map (Low . XCmd) 
	                  [ChangeWindowAttributes [CWBackPixmap none],
	                   ClearArea r e,
			   ChangeWindowAttributes [CWBackPixel bgcol]]
			-- Some backround may be lost if the windows becomes
			-- obscured while the BackPixmap is none !!!
      in if diff>0
	 then resizeK llcmd $ \ newwinsize ->
	      putsK (scrollcmd++clearcmd) $
	      textK (newwinsize - diag margin) newsize sel' mtxt'
	 else putsK (scrollcmd++clearcmd++llcmd) $
	      textK winsize newsize sel' mtxt'

    textK :: Size -> Size -> [Int] -> [(String,Int)] ->
             PK TextF TextRequest (InputMsg (Int,String))
    textK winsize@(Point winwidth _) size sel mtxt =
       -- winsize is the size of the window excluding the right & bottom margins
	getK $ message lowK (either paramChangeK textRequestK)
      where
        same = textK winsize size sel mtxt
	textRequestK msg =
	    case msg of
	      ReplaceItems dfrom dcnt newtxt ->
		replaceTextK winsize size sel mtxt dfrom dcnt newtxt
	      HighlightItems sel' ->
		changeHighlightK sel' $
		textK winsize size sel' mtxt
	      PickItem n -> output inputMsg n
	      _ -> same
	lowK event =
	    case event of
	      XEvt (ButtonEvent {pos=Point _ y, type'=press}) ->
		let l=y `quot` ls
		    pressmsg = case press of
				 MultiClick 2 -> inputMsg
				 _ -> InputChange
		in output pressmsg l
	      XEvt (Expose {rect=r}) ->
		redrawTextK r $
		same
	      XEvt (GraphicsExpose {rect=r}) ->
		redrawTextK r $
		same
	      LEvt (LayoutSize newwinsize) ->
	        textK (newwinsize - diag margin) size sel mtxt
	      _ -> same
	paramChangeK _ = same -- !!! Dynamic customisation not implemented yet
        output pressmsg l = (if l>=0 && l<length mtxt
	                     then putK (High (pressmsg (l,fst(mtxt!!l))))
			     else id) $ same

	changeHighlightK sel' =
	    putsK (mkvis++[Low $ wDrawMany (map draw changes)])
	  where
	    changed n = (n `elem` sel) /= (n `elem` sel')
	    nmtxt = number 0 mtxt
	    changes = [l | l@(n,_)<-nmtxt, changed n]
	    selected = [l | l@(n,_)<-nmtxt, n `elem` sel']
	    draw (n,(s,w)) = (dgc sel' n,[drimstr (Point (x0 w) (base+n*ls)) s])
	    mkvis =
	      case (selected,last selected) of -- needs lazy evalution!
		([],_) -> []
		((n1,(_,w1)):_,(n2,(_,w2))) ->
		    [Low (LCmd (layoutMakeVisible vrect))]
		  where vrect = rR x1 y1 (x2-x1+5) (y2-y1+5)
                        x1 = min (x0 w1) (x0 w2) -- !!! Should use min/max
			x2 = max (x0 w1) (x0 w2) -- !!! of all changes.
			y1 = n1*ls
			y2 = (n2+1)*ls

	redrawTextK r@(Rect (Point x y) (Point w h)) =
	  let first = (max 0 (y-margin)) `quot` ls
	      last = (y+h-1) `quot` ls
	      lines = number first (take (last-first+1) (drop first mtxt))
	      firsty = base+ls*first
	      ys = [firsty,firsty+ls..]
	  in putsK [Low $ XCmd $ ClearArea r False,
		    Low $ wDrawMany
	             [(dgc sel n,[drimstr (Point x1 ly) s]) | 
		     ((n,s,x1,x2),ly)<-zip (map xi lines) ys,x<x2 && (x+w)>=x1]]
		     -- !! The x coordnates should probably be stored
		     -- rather than recomputed every time the text is
		     -- redrawn...

        xi (n,(s,w)) = (n,s,x1,x2) where x1=x0 w; x2=x1+w
        x0 w = margin+floor (align*fromInt (winwidth-margin-w))
	       -- !!! Problem: can't be sure that bitgravity moves stuff
	       -- to the same pixel coordinates that are computed here...

    dgc sel n = if n `elem` sel -- inefficient !!
                then gcinv
		else gc

resizeK cmd cont = putsK cmd $ waitForMsg ans $ cont
  where ans (Low (LEvt (LayoutSize newsize))) = Just newsize
        ans _ = Nothing

doubleClickTime = 400 -- The double click timeout should not be hard wired like this...
textbg = colorSpec [bgColor,"white"]
textfg = colorSpec [fgColor,"black"]

horizAlignGravity align =
    case (align::Alignment) of
      0 -> NorthWestGravity
      0.5 -> NorthGravity
      1 -> NorthEastGravity
      _ -> ForgetGravity

--take' n | n>=0 = take n
