module MenuF(simpleMenuF, menuAltsF, menuF, oldMenuF, buttonMenuF, buttonMenuF', grabberF, MenuState,menuDown,EqSnd(..),fstEqSnd,sndEqSnd,toEqSnd) where
import Command
import Event
import Geometry(pP,Point(..),Size,inRect,Rect(..))
import Message(message )--Message(..),
import Fudget
import FRequest
import FudgetIO
import StreamProcIO
import Xcommand
import NullF
import CompOps((>^=<), (>=^<), (>=^^<), (>^^=<),(>+<))
import Dlayout(groupF)
import SerCompF(bypassF)--,idRightF
import Loops(loopCompThroughRightF)
import LayoutDir(LayoutDir(..))
import LayoutF(listLF)
import LayoutRequest(LayoutResponse(..))
import Placers
import Spacers() -- synonym Distance, for hbc
import MenuButtonF
import MenuPopupF
import Spops(nullSP)
import MapstateK
import SpEither(filterRightSP)
import EitherUtils(mapEither)--stripEither
import Xtypes
import Defaults(menuFont)
import CmdLineEnv(argFlag)
import Graphic
import Data.Array
--import DialogueIO hiding (IOError)
import ShowCommandF(showCommandF)
import Debug.Trace(trace)

data EqSnd a b = EqSnd a b

instance (Eq b) => Eq (EqSnd a b) where
    EqSnd a1 b1 == EqSnd a2 b2  =  b1 == b2

fstEqSnd (EqSnd a b) = a
sndEqSnd (EqSnd a b) = b
toEqSnd x = map (uncurry EqSnd) x

menuF :: (Graphic mlbl,Graphic albl) => mlbl -> [(alt,albl)] -> F alt alt
menuF name altlbls =
    bypassF ((nalts!) . sndEqSnd >^=< 
      simpleMenuF menuFont name lblns fstEqSnd >=^^< nullSP)
  where
    (alts,lbls) = unzip altlbls
    lblns = zipWith EqSnd lbls ixs
    n = length alts
    ixs =  [1 .. n]
    nalts = array (1,n) (zip ixs alts)


simpleMenuF fname name = oldMenuF fname name . map (\x -> (x,[]))

oldMenuF :: (Graphic c, Eq b, Graphic a) => FontName -> a -> [(b, [(ModState, KeySym)])] -> (b -> c) -> F a b 
oldMenuF fname name alts show_alt =
    grabberF [] (buttonMenuF Horizontal fname name alts menuAlts>=^<mapEither id Left)
  where
    menuAlts = menuAltsF' fname (map fst alts) show_alt >=^^< filterRightSP

menuAltsF' fname alts show_alt =
    fst >^=< listLF (verticalP' 0) (map altButton alts)
  where
    altButton (alt{-, keys-}) = (alt, menuButtonF fname {-keys-} (show_alt alt))

menuAltsF fname alts show_alt =
   menuPopupF (menuAltsF' fname alts show_alt) >=^< Left

grabberF alts mF = loopCompThroughRightF (groupF startcmds grabK0 mF)
  where
    startcmds = map XCmd transinit
    grabK0 = grabK False

    transinit =
        if null keys
	then []
        else  [TranslateEvent tobutton [KeyPressMask, KeyReleaseMask]]
      where
        keys = concatMap snd alts
        tobutton e@(KeyEvent {state=s,keySym=ks}) | (s, ks) `elem` keys = Just e
	tobutton _ = Nothing
    grabK up = getK $ message low high
      where
	keyalts = [(k,a)|(a,ks)<-alts,k<-ks]
	same = grabK up
	popdown = grabK False
	popup = grabK True
	low event =
	  case event of
	    XEvt (KeyEvent {state=s,keySym=ks,type'=Pressed}) ->
		puts [Left (Right alt)|(key,alt)<-keyalts,(s,ks)==key] same
	    _ ->  same
	high = either fromLoop fromOutside
	fromLoop = either menuCoordination menuSelection
	fromOutside x = putHigh (Left (Right x)) same
	menuSelection x = putHigh (Right x) same
	menuCoordination newState =
	  case (up,newState) of
	    (False,MenuUp _) ->
	      --trace "grabberF: GrabEvents False" $
	      xcommandK (GrabEvents False) popup
	    (True,MenuDown) ->
	      --trace "grabberF: UngrabEvents" $
	      xcommandK UngrabEvents popdown
	    _ -> same

data MenuState = MenuDown | MenuUp MenuMode deriving (Show)
type MenuMode = Bool -- True = sticky
menuDown = MenuDown
menuUpSticky = MenuUp True
menuUpMPopup = MenuUp False
-- Invariant: menu state never changes directly from MenuDown to menuUpSticky,
-- i.e., when a menu first pops up, it always outputs menuUpMPopup

data ButtonMenuState =
  S { mpopup,othermpopup,sticky,debug::Bool, size::Size } deriving (Show)
bstate0 = S False False False False 0

{-
buttonMenuF :: (Graphic a) =>
	 LayoutDir -> FontName -> a ->
	 [(b, [(ModState, KeySym)])] ->
	 F (Either MenuState b) b ->
	 F (Either MenuState (Either a b)) (Either MenuState b)
-}
buttonMenuF x = buttonMenuF' False x
buttonMenuF' delayed dir fname name alts menuAltsF =
    loopCompThroughRightF $
    showCommandF "buttonMenuF" $
    groupF startcmds
	   (mapstateK proc bstate0)
	   (filterRightSP >^^=< (menuLabelF fname name >+< theMenuF))
  where
    theMenuF = menuPopupF' delayed menuAltsF
    topopup = High . Left . Right . Left
    tosubmenus = High . Left . Right . Right . Left
    inputtosubmenus = High . Left . Right . Right . Right
    out = High . Right . Right
    othermenu = High . Right . Left
    toDisp = High . Left . Left
    relabel = toDisp . Right
    adjust =
	case dir of
	  Vertical -> \ (Point w _) -> pP w (-1)
	  Horizontal -> \ (Point _ h) -> pP (-1) h
    proc state@(S{mpopup=mpopup,othermpopup=othermpopup,sticky=sticky,size=size,debug=debug}) =
	message low high
      where
	dbg x = if debug then trace ("buttonMenuF "++x) else id

	popdownyield = popdown' True [] --pop down because other menu popped up
	popdownlast = -- pop down, no other menu is up
	  dbg "popdownlast" $
	  popdown' False [othermenu MenuDown]
	popdown' mpopup' msgs =
	  (state{othermpopup=mpopup'},
	   msgs++[tosubmenus menuDown,topopup PopdownMenu])

	stickyMode =
	  dbg "othermenu menuUpSticky" $
	  (state{sticky=True},
	   [othermenu menuUpSticky,topopup PopupMenuStick])

	mPopupMode b = (state{mpopup=b},[])
	highlight = toDisp . Left
	put msgs = (state,msgs)

	high = either fromMenu (either fromOtherMenu fromOutside)
	fromOutside = either newLabel altInput
	newLabel lbl = (state,[relabel lbl])
	altInput x = (state,[inputtosubmenus x])

	fromOtherMenu newMode =
	  dbg ("fromOtherMenu "++show newMode) $
	  case newMode of
	    MenuUp False -> popdownyield -- other menu popped up, pop down
	    MenuUp True -> (state{othermpopup=False},[])
	    MenuDown -> popdown' False []

	fromMenu alt =
	  (state{sticky=False},
	   [tosubmenus menuDown,othermenu menuDown,out alt])
	low resp =
	  dbg (unlines [show state, show resp,""]) $
	  case resp of
	    XEvt event ->
	      case event of
		ButtonEvent {button=Button 2,type'=Pressed,state=mods} ->
		  trace "Button 2" $
		  (state{debug=Control `elem` mods},[])
		--ButtonEvent _ winpos rootpos mods Pressed (Button 1) ->
		ButtonEvent {pos=winpos,rootPos=rootpos,state=mods,type'=Pressed,button=Button 1} ->
		  dbg "output othermenu True" $
		  (state{sticky=False},
		   [othermenu menuUpMPopup, -- tell other menus to pop down
		    topopup (PopupMenu (rootpos-winpos+adjust size) event)
		    --highlight True,
		    --Low (GrabEvents False)
		    ])
		LeaveNotify {mode=NotifyUngrab,detail=NotifyInferior}
		    | stickyMenus && not mpopup -> stickyMode
		LeaveNotify {mode=NotifyUngrab} {-  | not sticky-} -> popdownlast
		--  ^^ these events get lost in focusMgr it seems
		ButtonEvent {pos=pos,button=Button 1,type'=Released}
		    | not (stickyMenus && pos `inRect` (Rect 0 size)) -> popdownlast
			     --workaround
		LeaveNotify {detail=detail}
		    | detail/=NotifyInferior ->
			if False --mpopup
			then popdownlast
			else put [highlight False]
		EnterNotify {rootPos=rootpos,pos=winpos,mode=NotifyNormal}
		  | mpopup || othermpopup ->
		      dbg "output othermenu True" $
		      (state{sticky=False},
		       [othermenu menuUpMPopup, -- tell other menus to pop down
			topopup (PopupMenu (rootpos-winpos+adjust size) event),
			highlight True])
		  | otherwise -> put [highlight True]
		KeyEvent {state=s,type'=Pressed,keySym=ks} ->
		  case [ a | (a,keys) <- alts, (s,ks) `elem` keys] of
		    a:_ -> put [out a]
		    _ -> error "MenuF.clickF bug"
		MenuPopupMode b -> mPopupMode b
		_ -> (state,[])
	    LEvt (LayoutSize size') -> (state{size=size'},[])
	    _ -> (state,[])

    startcmds = map XCmd (MeButtonMachine : grab ++
	                  [ConfigureWindow [CWBorderWidth 1],
			   ChangeWindowAttributes wattrs] ++
			  transinit)
    grab = [GrabButton True (Button 1) [] ptrmask]
    ptrmask = [ButtonPressMask, ButtonReleaseMask]
    wattrs = [CWEventMask eventmask]
    eventmask = [LeaveWindowMask, EnterWindowMask,
		 ButtonPressMask -- Button 2 press, for debuggin only!
		]

    keys = concatMap snd alts

    transinit =
        if null keys
	then []
        else  [TranslateEvent tobutton [KeyPressMask, KeyReleaseMask]]
      where
        tobutton e@(KeyEvent {state=s,keySym=ks}) | (s, ks) `elem` keys = Just e
	tobutton _ = Nothing

stickyMenus = argFlag "stickymenus" False
