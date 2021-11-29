module Editor(oldEditorF,selectall,loadEditor,setEditorCursorPos) where
import Command
import CompOps((>+<))
import Cont(cmdContK')
import Cursor
import Defaults(menuFont,bgColor)--defaultFont
import CmdLineEnv(argReadKey)
import QueryPointer
import TimerF
import Dlayout(groupF)
import Edtypes
import Edit--(EditStopT(..), EditCmd(..), EditEvt(..), IsSelect(..), editF)
import Event
--import Font(FontStruct)
import Fudget
import FRequest
import Geometry() -- instances, for hbc
import LayoutRequest
import Loops(loopCompThroughRightF)
--import Message(Message(..))
import NullF
--import Path(Path(..))
import PopupMenuF
--import SP
import SelectionF
--import Utils(mapList)--loop,
import Xtypes
import Data.Char(isAlpha,toLower,isPrint)
--import Graphic
import InputMsg(InputMsg(..))

default(Int) -- mostly for Hugs

ems = EditMove . EditStopFn
stopafter n dir = ems (sa n) 
	  where sa n b a = if (n::Int) <= 0 then EdStop else EdGo dir (sa (n-1))
stop1 = stopafter 1

ifhd p l = not (null l) && p (head l)
aheadl dir b a = if dir == ELeft then b else a
ifdirhd p dir b a = ifhd p (aheadl dir b a)
stopwhen p dir = ems sw
   where sw b a = if ifdirhd p dir b a then EdStop
	          else EdGo dir sw
stopat c = stopwhen (==c)
stopnl = stopat newline

stopborder p dir = ems sw
  where sw b a = if ifdirhd (not . p) dir b a &&
		    ifdirhd p dir a b then EdStop
		 else EdGo dir sw

stopword = stopborder isAlpha

neverstop = stopwhen (const False)

-- replaceAll is used for TextRequests
loadEditor s = selectall++[EditReplace s]
selectall = [neverstop ELeft False,
	     neverstop ERight True]

setEditorCursorPos (row,col) =
   EditMove (EditPoint 0) False :
   concat (replicate (row-1) (down `funmap` False)) ++
   [stopafter (col-1) ERight False]

horiz dir meta = stop1 dir : if meta then [stopword dir] else []
left = horiz ELeft
right = horiz ERight
up = [EditMove (EditLine ELeft)]
down = [EditMove (EditLine ERight)]

undo = [const EditUndo]
redo = [const EditRedo]

cursorbindings meta = 
    [("left", left meta), 
     ("right",right meta),
     ("up",up),
     ("down",down),
     ("b",left meta),
     ("f",right meta)] 

ctrls  = [("e",[stopnl ERight]),
         ("a",[stopnl ELeft]),
	 ("p",up),
	 ("n",down),
	 ("b",left False),
	 ("f",right False),
	 ("slash",undo),
	 ("question",redo)]

selectleft meta = funmap (left meta) True
fl `funmap` x = [f x | f <- fl]
hasMeta mods = Mod1 `elem` mods
hasControl mods = Control `elem` mods
cursorkey mods key = flip lookup (if hasControl mods then ctrls 
			     else cursorbindings (hasMeta mods))
			    (map toLower key) 
		     >>= \l-> Just (funmap l (Shift `elem` mods))

isEnterKey key = key == "Return" || key == "KP_Enter"
printorenter mods key ascii =
    if hasMeta mods then Nothing
    else if isEnterKey key then
        Just [newline]
    else if key == "Tab" then Just ['\t']
    else if not (hasControl mods) then
        case ascii of
          c : _ | isPrint c -> Just [c]
          _ -> Nothing
   else Nothing
toEdF = High . Left . Right . Right

toSelF = High . Left . Right . Left . Left
toTimerF = High . Left . Right . Left . Right

toOut = High . Right

getEdSel = getEd EditGetSelection
getEdText = getEd EditGetText

getEd ecmd =
    cmdContK' (toEdF ecmd)
              (\e ->
               case e of
                 High (Left (Right (Right (EditText t)))) -> Just t
                 _ -> Nothing)

getSel =
    cmdContK' (toSelF PasteSel)
              (\e ->
               case e of
                 High (Left (Right (Left (Left (SelNotify t))))) -> Just t
                 _ -> Nothing)

replace' s = putK (toEdF $EditReplace s)
clearSel = replace' ""
copySel k = getEdSel $ (\s -> putK (toSelF (Sel s)) k)
click issel p = putK (toEdF $ EditMove (EditPoint p) issel)
starttimer = putK (toTimerF $ Just (scrolldel,scrolldel))
stoptimer = putK (toTimerF $ Nothing)
scrolldel = argReadKey "scrolldel" 200

oldEditorF font = loopCompThroughRightF g where
   g = groupF (map XCmd [ChangeWindowAttributes 
	       [CWEventMask [KeyPressMask,EnterWindowMask,LeaveWindowMask]],
	       ConfigureWindow [CWBorderWidth 1],
	       GrabButton True (Button 1) [Any] 
	          [ButtonPressMask,PointerMotionMask,ButtonReleaseMask]])
	      (setFontCursor 152 $ editorK False False)
	      (menu ((selectionF >+< timerF) >+< editF font))
   editorK bpressed focus = same where
     same = 
      getK $ \msg ->
        case msg of
         Low (XEvt event) ->
	   case event of
	     KeyEvent _ _ _ mods Pressed _ key ascii -> 
		if hasMeta mods && isEnterKey key
		then putInputDoneMsg key
		else
		case printorenter mods key ascii of
		   Just s -> replace' s same
		   Nothing -> case cursorkey mods key of
		      Just eds -> putsK (map toEdF eds) same
		      Nothing -> 
			 if key `elem` ["Delete","BackSpace"] 
			 then getEdSel $ \s -> 
			      (if null s 
			       then putsK (map toEdF 
					      (selectleft (hasMeta mods)))
			       else id) $ clearSel same
			 else same
	     MotionNotify {pos=p,state=mods} -> click True p same
	     ButtonEvent {pos=p,state=mods,type'=Pressed,button=Button 1} ->
		starttimer $
		click (Shift `elem` mods) p $ showCursor True focus
	     ButtonEvent {type'=Released,button=Button 1} -> 
		stoptimer $ showCursor False focus
	     FocusIn {mode=NotifyNormal} -> showCursor bpressed True
	     FocusOut {mode=NotifyNormal} -> showCursor bpressed False
	     _ -> same
	 High (Left (Right (Left (Right Tick)))) ->
	   queryPointerK $ \(_,_,p,_) -> click True p same
	 High (Left (Left mencmd)) -> case mencmd of
				      MenCut -> copySel $ clearSel same
				      MenCopy -> copySel same
				      MenPaste -> getSel $ \s ->
						  replace' s same
	 High (Left (Right (Right ecmd))) -> 
	    (case ecmd of
		  EditCursor r -> putK (Low $ LCmd $
					layoutMakeVisible r)
		  _ -> id) $
	    putK (toOut ecmd) same
	 High (Right ocmd) -> putK (toEdF ocmd) same
	 _ -> same
     showCursor b f = putK (toEdF (EditShowCursor (b || f))) $ 
		    editorK b f
     putInputDoneMsg key =
       getEdText $ \ s ->
       putK (toOut $ EditChange (InputDone key s)) $
       putsK (map toEdF selectall) $
       same

data MenEvt = MenCut | MenCopy | MenPaste  deriving (Eq, Ord)

menu = oldPopupMenuF bgColor True menuFont (Button 3) [] [] 
     [(MenCut, []), (MenCopy, []), (MenPaste, [])]
               (\x -> case x of
                  MenCut -> "Cut"
                  MenCopy -> "Copy"
                  MenPaste -> "Paste")
