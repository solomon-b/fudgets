{-# LANGUAGE CPP #-}
module TagEvents(tagEventsSP) where
import Command
import CompSP(preMapSP,serCompSP)
import SpEither(mapFilterSP)
import Cont(cmdContSP)
import CmdLineEnv(argFlag)
--import Direction
import Event
--import Font(FontStruct)
import Fudget
import FRequest
--import Geometry(Line, Point, Rect, Size(..))
import IOUtil(getEnvi)
--import LayoutRequest(LayoutRequest)
import Loopthrough
import Message(stripLow) --Message(..),
import Path
import WindowF(autumnize)
import ShowCommandF
import Sockets
import Spops
import Tables
--import Version
import Xtypes
--import Maptrace
--import EitherUtils
import Data.Maybe(isNothing)
import ShowFailure
import DialogueIO
import Prelude hiding (IOError)

--mtrace = ctrace "tagEvents"
mtrace x y = y

tagEventsSP :: F i o -> SP (Path, Response) (Path, Request)
tagEventsSP mainF =
    loopThroughRightSP
      tagEventsCtrlSP
      (mapFilterSP stripLow `serCompSP` mainFSP `preMapSP` Low)
  where
    F mainFSP = traceit mainF

openDisplay' cont =
    if isNothing (getEnvi "DISPLAY")
    then cont faildisp
    else
    cmdContSP (tox $ XRequest (noDisplay, noWindow, OpenDisplay ""))
              (\e ->
               case e of
                 Right (_, XResponse (DisplayOpened d)) -> Just d
                 Right (_, Failure f) -> error ("Cannot open the display (the program is probably not linked with the X routines): "++showFailure f)
                 _ -> Nothing)
              (\disp ->
               if disp == Display 0 then
                   error "Cannot open display"
               else
                   putSP (tox $ Select [DisplayDe disp]) $ cont disp)
  where faildisp = error "the environment variable DISPLAY is not set!"
        tox x = Right (here,x)

tagEventsCtrlSP::
    SP (Either TCommand (Path,Response)) (Either TEvent (Path,Request))
tagEventsCtrlSP =
    openDisplay' tagEventsCtrlSP'
  where
    tagEventsCtrlSP' disp =
	tagSP noSel Nothing path2wid0 wid2path0
      where
	noSel = here
	tagSP selp grabpath path2wid wid2path =
	  let same = tagSP selp grabpath path2wid wid2path
	      tagSPs = tagSP selp
	      tagSPns s = tagSP s grabpath path2wid wid2path
	  in getSP $ \msg -> case msg of
	    Left (path', cmd) ->
	      let newwindow path'' wid = 
		    putSP (Left (path'', XResp (WindowCreated wid))) $
		    tagAdd path'' wid
		  tox xc = Right (path',xc)
		  convertcmd = convert (lookupWid path2wid path')
		  convert w cmd = putSP (tox (XCommand (disp, w, cmd)))
		  tagAdd p w = tagSPs grabpath (updateWid path2wid p w) 
					       (updatePath wid2path w p)
	      in case cmd of
		 XCmd xcmd@(SetSelectionOwner s atom) ->
		 -- currently, different selections are not distinguished
		   convertcmd xcmd $ 
		   (if s && selp /= noSel && path' /= selp then 
		       putSP (Left (selp,XEvt (SelectionClear atom))) else id) $
		   tagSPns (if s then path' else noSel)
		 XCmd (ReparentToMe rchild w) -> 
		   -- lookup w in table, change path to rchild, emit reparent cmd
		   -- TODO: change subpaths too!
		   let npath' = newChildPath path' rchild
		       npath = autumnize npath' -- used in repTest (?)
		       wpath = lookupPath wid2path w
		       opath = autumnize wpath
		       nparent = lookupWid path2wid path'
		       npath2wid = moveWids path2wid opath npath
		       nwid2path = movePaths wid2path opath npath
		   in convert w (ReparentWindow nparent) $
		      if null wpath
		      then {-ctrace "rep" (npath',opath,w) $-} tagAdd npath' w
		      else tagSPs grabpath npath2wid nwid2path
		 XCmd (SelectWindow w) -> tagAdd path' w
		 XCmd GetWindowId -> putSP (Left (path',XEvt (YourWindowId wid))) same
		     where wid = lookupWid path2wid path'
		 XCmd DestroyWindow ->
		   putsSP [tox (XCommand (disp, wid, DestroyWindow))
			  | wid <- subWids path2wid path']
			 (tagSPs grabpath (pruneWid path2wid path') wid2path)
		 XCmd (GrabEvents toMe) -> mtrace ("Grab",toMe,msg) $
		   tagSPs (Just (toMe,path',autumnize path')) path2wid wid2path
		 XCmd UngrabEvents -> tagSPs Nothing path2wid wid2path
		 --DoXCommands xcmds -> foldr convertcmd same xcmds
		 XCmd (DrawMany w gcdcmdss) | not optimizeDrawMany ->
		    foldr convertcmd same
		      [Draw w gc dcmd | (gc,dcmds)<-gcdcmdss,dcmd<-dcmds]
		 XCmd xcmd -> convertcmd xcmd same
		 DReq req -> putSP (tox req) same
		 SReq sreq -> putSP (tox (SocketRequest sreq)) same
		 XReq xreq ->
		   case xreq of
		     CreateMyWindow _ -> error "GUI fudget outside a shell fudget"
		     CreateSimpleWindow rchild _ ->
			createWindow disp xreq (lookupWid path2wid path')
				      (newwindow (newChildPath path' rchild))
		     CreateRootWindow _ _ -> 
			 createWindow disp xreq rootWindow (newwindow path')
		     _ -> putSP (tox (XRequest (disp, 
				  lookupWid path2wid path', xreq))) same
		 LCmd _ -> same -- layout pseudo command
	    Right (path', resp) -> case resp of
	      AsyncInput (_, XEvent (wid, event)) ->
		case event of
		  MappingNotify -> same
		  ButtonEvent {} -> checkGrab
		  KeyEvent {} -> checkGrab
		  MotionNotify {} -> checkGrab
		  SelectionClear atom -> pass $ tagSPns noSel
		  DestroyNotify w -> if argFlag "destroyPrune" False then
		     pass $ tagSPs grabpath path2wid' (prunePath wid2path w)
		    else passame
		    where path2wid' = if null path2' then path2wid
						    else pruneWid path2wid path2'
		  _ -> passame
		where path2' = lookupPath wid2path wid
		      passto p = putSP (Left (p, XEvt event))
		      pass = passto path2'
		      passame = pass same
		      checkGrab = case grabpath of
				    Nothing -> passame
				    Just (toMe,kpath,path) -> 
				      if path `subPath` path2' then passame
				      else if toMe then passto kpath same
					   else same
              XResponse xresp -> putSP (Left (path',XResp xresp)) same
	      SocketResponse sresp -> putSP (Left (path',SResp sresp)) same
	      _ -> putSP (Left (path', DResp resp)) same

newChildPath parent rchild = absPath (autumnize parent) rchild
  
createWindow disp xreq wid cont =
    cmdContSP (Right (here, XRequest (disp, wid, xreq)))
              (\msg -> case msg of
                         Right (_, XResponse (WindowCreated wid')) -> Just wid'
                         _ -> Nothing)
              cont

traceit = showCommandF "debug"

optimizeDrawMany =
  argFlag "optdrawmany"
#ifdef __GLASGOW_HASKELL__
    True
#else
    False
#warning "not optimising DrawMany"
#endif
