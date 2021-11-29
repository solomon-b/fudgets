{-# LANGUAGE CPP #-}
module StringF(
  stringF'',StringF,
  {-HasBorderWidth(..),HasAllowedChar(..),HasShowString(..),-}
  getAllowedChar,setAllowedChar,getShowString,setShowString,
  setInitStringSize,
  getCursorPos,setCursorPos,getInitString,setInitString,
  generalStringF, oldIntF, oldPasswdF, oldStringF, bdStringF, oldGeneralStringF
  ) where
import BgF(changeGetBackPixel)
--import Color
import Command
import DrawInWindow
import CompOps((>=^<), (>^=<))
--import Utils(bitand)
import HbcWord
import Cursor
import Defaults(defaultFont, inputFg, inputBg)
import CmdLineEnv(argKey, argKeyList)
import Dlayout(windowF)
import Event
import Font(split_string,font_ascent,next_pos,linespace,font_id,string_box_size,font_range)
import Fudget
--import FudgetIO
import FRequest
import Xcommand
import Gc
import Xtypes
import Geometry(Point(..), pP, rR,pmax)
import LayoutRequest(plainLayout,LayoutResponse(..))
--import LoadFont
--import Message(Message(..))
import NullF
--import Spops
import StringEdit
import InputMsg(InputMsg(..),mapInp,inputLeaveKey)
import InputF(InF(..))
import SelectionF
import Loops(loopThroughRightF)
import Sizing
#ifdef __GLASGOW_HASKELL__
import FDefaults hiding (setInitSize,getInitSize,getInitSizeMaybe)
#else
-- Some versions of HBC fail if you mention a constructor class in an import spec.
--import FDefaults hiding (HasInitSize)
import FDefaults(cust,getpar,getparMaybe,HasBorderWidth(..),HasSizing(..),HasBgColorSpec(..),HasFgColorSpec(..),HasFontSpec(..),Customiser(..),PF(..))
#endif
import Data.Char(isPrint,isDigit)
import GCAttrs --(ColorSpec,colorSpec,convColorK) -- + instances

default(Int)

-- chr/ord are defined in *some* versions of the library module Char...
chr' = toEnum . wordToInt :: (Word->Char)
ord' = fromIntegral . fromEnum :: (Char->Word)


#include "defaults.h"

newtype StringF = Pars [Pars]

parameter(AllowedChar)
parameter(ShowString)
parameter(CursorPos)
parameter(InitString)

parameter_instance(BorderWidth,StringF)
parameter_instance(FgColorSpec,StringF)
parameter_instance(BgColorSpec,StringF)
parameter_instance(FontSpec,StringF)
parameter_instance(Sizing,StringF)
--parameter_instance(InitSize,StringF) -- StringF has wrong kind for this
parameter(InitSize)

setInitStringSize = setInitSize -- avoid name clash

data Pars
  = BorderWidth Int
  | FgColorSpec ColorSpec
  | BgColorSpec ColorSpec
  | FontSpec FontSpec
  | AllowedChar (Char->Bool)
  | ShowString (String->String)
  | InitSize String
  | Sizing Sizing
  | CursorPos Int -- puts cursor after the nth character
  | InitString String

isTerminator key = key `elem` ["Escape", "Return", "KP_Enter", "Tab", "Up", "Down"]

isBackSpace (c : _) = c == '\BS' || c == '\DEL'
isBackSpace _ = False

ctrl c = chr' (bitAnd (ord' c) (65535-96))

isCtrl c (c':_) = c' == ctrl c
isCtrl _ _      = False

isKill = isCtrl 'u'

modchar mods c0 = if Mod1 `elem` mods then chr' (ord' c0 `bitOr` 128) else c0

cursorBindings' =
  [(([], "Left"), moveCursorLeft),
   (([], "Right"), moveCursorRight),
   (([], "Home"), moveCursorHome),
-- (([], "Up"), moveCursorHome),
   (([], "End"), moveCursorEnd),
-- (([], "Down"), moveCursorEnd),
--   (([Shift],"Control"), moveCursorHome), -- ???
--   (([Shift],"Control"), moveCursorEnd), -- ???
   (([Shift],"Left"), extendCursorLeft),
   (([Shift],"Right"), extendCursorRight),
   (([Shift],"Home"), extendCursorHome),
   (([Shift],"Up"), extendCursorHome),
   (([Shift],"End"), extendCursorEnd),
   (([Shift],"Down"), extendCursorEnd)]
   ++ emacsBindings

emacsBindings = 
  [(([Control], "b"), moveCursorLeft),
   (([Control], "f"), moveCursorRight),
   (([Control], "e"), moveCursorEnd),
   (([Control], "a"), moveCursorHome)]

cursorKey' mods key = lookup (filter (<=Mod5) mods,key) cursorBindings'

hmargin = 3
vmargin = 2

placecursor font (Point x _) field =
    case getField field of
      [] -> field
      cs -> let (lcs, rcs, _) = split_string font cs (x - hmargin)
            in  createField2 (lcs, rcs)

showinputfield gc gcinv font show' = showinputfield'
  where
    drimstr = if snd (font_range font) > '\xff'
              then wDrawImageString16
	      else wDrawImageString

    showinputfield' active field =
      let y = font_ascent font + 1
	  draw x s = if null s then [] else [drimstr gc (pP x y) s]
	  showpart gc' s0 (x, cmds) =
	      let s = show' s0
	      in  (x + next_pos font s, draw x s ++ cmds)
	  showcursor s (x1, cmds) =
	      let (x2, cmds') = showpart gc s (x1, cmds)
		  cmd = if active
			then [wFillRectangle gcinv
					     (rR (x1 - 1) 1
						 (x2 - x1 + 1) (linespace font))]
		        else []
	      in  (x2, cmds' ++ cmd)
      in  snd (showField (showpart gc) showcursor field (hmargin, []))

createField' pos s =
  if pos<0
  then createField s
  else createField2 (splitAt pos s)

stringK bw initsize sizing bgcolor fgcolor fontspec allowedchar show' cursor defaultText active =
    setFontCursor 152 $
    xcommandK (ConfigureWindow [CWBorderWidth bw]) $
    changeGetBackPixel bgcolor $ \bg ->
    convColorK fgcolor $ \fg ->
    convFontK fontspec $ \ fd ->
    fontdata2struct fd $ \ font ->
    wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id font),
                      GCForeground fg, GCBackground bg] $ \drawGC ->
    wCreateGC rootGC (invertColorGCattrs bg fg) $ \invertGC ->
    let drawit field active = map Low (XCmd ClearWindow : drawcmds)
          where drawcmds = shinpf active field
	shinpf = showinputfield drawGC invertGC font show'
	stringproc size' field active =
	   let redraw f =
	         putsK (drawit f active) (stringproc size' f active)
	       nochange = stringproc size' field active
	       newsize s = stringproc s field active
	       changeactive a = putsK (drawit field a) $
	                        stringproc size' field a
	       emit msg f a = putsK (drawit f a ++ [High (Right msg)]) $
	                      stringproc size' f a
	       emitchange f =
	          let gf = getField f
		  in  updlayout size' gf (emit (InputChange gf) f active)
	       emitdone key f = emit (InputDone key (getField f)) f active
	       emitleave =
	         emit (InputDone inputLeaveKey (getField field)) field False
	       paste = putK (High (Left PasteSel)) nochange
	       copy = putK (High (Left (Sel (getField field)))) nochange
	   in getK $ \msg ->
	      case msg of
	        Low (XEvt event) ->
		  case event of
	            Expose _ _ -> redraw field
		    KeyEvent _ _ _ mods Pressed _ key ascii ->
		      case ascii of
			c0 : _ | allowedchar c -> ec (insertItem field c)
			   where c = modchar mods c0
			_ | isTerminator key ->
			       emitdone key (createField (getField field))
			  | isBackSpace ascii -> ec (deleteItemLeft field)
			  | isCtrl 'd' ascii  -> ec (deleteItemRight field)
			  | isCtrl 'k' ascii  -> ec (deleteToEnd field)
			  | isCtrl 'y' ascii  -> paste
			  | isCtrl 'c' ascii  -> copy
			  | isCtrl 'w' ascii  -> copy -- should acutally be cut
			  | isKill     ascii  -> ec (createField "")
			  | otherwise ->
			       case cursorKey' mods key of
				 Just ed -> redraw (ed field)
				 _ -> case key of
					"SunPaste" -> paste
					"SunCopy" -> copy
					_ -> --putK (Low (Bell 0)) $
					     nochange
		       where ec = emitchange
	            ButtonEvent {pos=p, button=Button 1} ->
		      redraw (placecursor font p field)
	            ButtonEvent {button=Button 2} -> paste
		    FocusIn  {} -> changeactive True
		    FocusOut {} -> emitleave
                    _ -> nochange
		Low (LEvt (LayoutSize nsize)) -> newsize nsize
		High (Right (Right newtext)) ->
		   if newtext/=getField field
		   then emitchange (createField newtext)
		   --else updlayout size' newtext (redraw (createField newtext))
		   else nochange
		High (Right (Left customiser)) ->
		  reconfigure customiser field active
		High (Left (SelNotify cs)) ->
		     emitchange (insertItemsSelected field s)
		   where s = filter allowedchar cs
		_ -> nochange

	reconfigure pmod field active =
	    -- !! unload fonts, free GCs & colors...
	    stringK bw' initsize' sizing' bgcolor' fgcolor' fontspec' allowed' show'' cursor' txt' active
	    -- !!! Bad: active will be reset to False.
	    -- !! A new layout request will be output (useful if font changed).
	  where ps = pmod (Pars [BorderWidth bw,
                                 BgColorSpec bgcolor,
				 FgColorSpec fgcolor,
				 FontSpec fontspec,
				 AllowedChar allowedchar,
				 ShowString show',
				 CursorPos (-1), -- !!
				 InitSize initsize,
				 Sizing sizing])
		bw' = getBorderWidth ps
		initsize' = txt' --getInitSize ps -- hmm !!
		sizing' = getSizing ps
		bgcolor' = getBgColorSpec ps
		fgcolor' = getFgColorSpec ps
		fontspec' = getFontSpec ps
		allowed' = getAllowedChar ps
		show'' = getShowString ps
		txt' = getField field
		cursor' = getCursorPos ps

	sizetext text = pP (2*hmargin) (2*vmargin) + string_box_size font text
	size = pmax (sizetext defaultText) (sizetext initsize)
	updlayout curSize gf =
	   let reqSize = sizetext gf
	       nsize = newSize sizing curSize reqSize
	   in if nsize /= curSize then putlayoutlims nsize else id
	putlayoutlims size' =
	   putK (Low (layoutRequestCmd (plainLayout size' False True)))
    in putlayoutlims size $
       stringproc size (createField' cursor defaultText) active

generalStringF bw initsize sizing bg fg fontspec allowedchar show' cursor txt =
   loopThroughRightF winF selectionF
  where
    eventmask = [ExposureMask, KeyPressMask, ButtonPressMask,
		 EnterWindowMask, LeaveWindowMask -- to be removed
		 ]
    startcmds = [XCmd $ 
                 ChangeWindowAttributes [CWBitGravity NorthWestGravity,
					 CWEventMask eventmask
					 {-,CWBackingStore Always-}]
		]
    winF = windowF startcmds 
	           (stringK bw initsize sizing bg fg fontspec
		            allowedchar show' cursor txt False)

stringF'' :: (Customiser StringF) -> PF StringF String (InputMsg String)
stringF'' pmod = generalStringF bw initsize sizing bg fg font allowed show cursor initstring
  where
    ps = pmod (Pars [BorderWidth 1,
                     BgColorSpec inputbg,
		     FgColorSpec inputfg,
		     FontSpec stringfont,
		     AllowedChar isPrint',
		     ShowString id,
		     InitSize "xxxxx",
		     Sizing Growing,
		     CursorPos (-1),
		     InitString ""])
    bw = getBorderWidth ps
    bg = getBgColorSpec ps
    fg = getFgColorSpec ps
    font = getFontSpec ps
    allowed = getAllowedChar ps
    show = getShowString ps
    --initsize = "xxxxx"
    initsize = getInitSize ps
    sizing = getSizing ps
    cursor = getCursorPos ps
    initstring = getInitString ps


oldGeneralStringF bw sizing font allowed show txt =
  generalStringF bw "xxxxx" sizing inputbg inputfg font allowed show (-1) txt >=^< Right

bdStringF bw dyn font = oldGeneralStringF bw dyn font isPrint' id

oldStringF :: String -> InF String String
oldStringF = bdStringF 1 Growing stringfont

oldPasswdF :: String -> InF String String
oldPasswdF = oldGeneralStringF 1 Static stringfont isPrint' (map (const '*'))

oldIntF :: Int -> InF Int Int
oldIntF default' =
    mapInp read >^=<
    oldGeneralStringF 1 Static stringfont isDigit id (show default') >=^<
    show

stringfont = fontSpec (argKey "inputfont" defaultFont)
inputbg = colorSpec (argKeyList "stringbg" [inputBg])
inputfg = colorSpec (argKeyList "stringfg" [inputFg])

-- Workaround limitations of HBC's Char.isPrint to allow Unicode input.
isPrint' c = c>'\xff' || isPrint c
