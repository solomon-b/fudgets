module SelectionF where 
import FudUTF8(decodeUTF8,encodeUTF8)
import Command
import CompOps((>=^<), (>^=<))
import Cont(conts,cmdContK')
import Shells(unmappedShellF)
import Event
import Fudget
import FRequest
import Xcommand
import GetWindowProperty
import InternAtom
--import Message(Message(..))
import NullF
import LayoutF(nullLF)
import Spops(putSP,getSP)
import Loops(loopThroughRightF)
import EitherUtils(stripEither)
import SerCompF(absF)
import Xtypes

{- 
Supports cut/paste of UTF-8 encoded Unicode Strings.
Cut/paste of Unicode strings between two fudgets program works.
Cut/paste between a fudget program and xterm -u8 from XFree86 4.0 works.
/TH 2000-04-02
-}

data SelCmd a = Sel a | ClearSel | PasteSel  deriving (Eq, Ord)
data SelEvt a = LostSel | SelNotify a  deriving (Eq, Ord)

data ESelCmd a = OwnSel | SelCmd (SelCmd a) deriving (Eq, Ord)
data ESelEvt a =  WantSel | SelEvt (SelEvt a) deriving (Eq, Ord)

eselectionF :: F (ESelCmd String) (ESelEvt String)
eselectionF =
    (stripEither >^=< unmappedShellF [] selK nullLF) >=^<
    Left where
 selK =
    conts (flip internAtomK True) 
      ["PRIMARY", "STRING", "NONE", "ATOM"] $ 
      \ [primaryA, stringA, noneA, atomA] ->
    conts (flip internAtomK False) ["FUDGETS_UTF8","UTF8_STRING"] $
      \ [fudgetsA, utf8A] -> let
      sevt = High. SelEvt
      l =
	  getK $ \ev ->
	  case ev of
	    High esc -> case esc of
	      SelCmd sc -> case sc of
		 Sel t -> l -- select t
		 ClearSel -> deselect
		 PasteSel -> paste_utf8string -- try UTF-8 first...
	      OwnSel -> select
	    Low (XEvt ev) -> case ev of
	      SelectionClear s | s == primaryA -> putK (sevt LostSel) l
	      SelectionRequest t w s -> selectionrequest t w s
	      SelectionNotify t s -> selectionnotify s
	      _ -> l
	    Low _ -> l
      selectionrequest time w sel@(Selection s t p) =
	if t `notElem` [stringA,utf8A]
	then notify time w (Selection s noneA p) l
	else
	  let p' = if p == noneA then t else p
	      wait (High (SelCmd (Sel t))) = Just t
	      wait _ = Nothing
	  in cmdContK' (High WantSel) wait $ \rawtext ->
	     let text = if t==utf8A
	                then encodeUTF8 rawtext
			else rawtext in
	     xcommandK (ChangeProperty w p' t 8 propModeReplace text) $
	     notify time w (Selection s t p') l
      notify t w sel = xcommandK (SendEvent w False [] (SelectionNotify t sel))
      paste_string = paste' stringA
      paste_utf8string = paste' utf8A
      paste' typ =
	  xcommandK (ConvertSelection (Selection primaryA typ fudgetsA)) l
      paste_failed = putK (sevt (SelNotify "")) l
      selectionnotify sel@(Selection s t p) =
          if p==noneA
	  then if t==utf8A  -- UTF8_STRING wasn't supported, try STRING
	       then paste_string
	       else paste_failed
	  else if t `notElem` [stringA,utf8A]
	       then paste_failed
	       else getWindowPropertyK 0 p True t $ 
			  \(typ, format, nitems, after,seltext) ->
		    let s' = if t==utf8A then decodeUTF8 seltext else seltext in
		    putK (sevt (SelNotify s')) l
      select = select' True
      deselect = select' False
      -- should check that setselectionowner succeeded.
      select' b = xcommandK (SetSelectionOwner b primaryA) l
    in l

selectionF :: F (SelCmd String) (SelEvt String)
selectionF = loopThroughRightF (absF (selSP "")) (eselectionF) where
  selSP text = 
    let same = selSP text 
	toesel = Left
	toout = Right in
    getSP $ \msg -> case msg of
	Right ocmd -> case ocmd of
	   Sel t -> putSP (toesel OwnSel) $ selSP t
	   _ -> putSP (toesel (SelCmd ocmd)) same
	Left esevt -> case esevt of
	   WantSel -> putSP (toesel (SelCmd (Sel text))) same
	   SelEvt se -> putSP (toout se) same
