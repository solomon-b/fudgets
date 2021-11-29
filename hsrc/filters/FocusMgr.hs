module FocusMgr(focusMgr) where
import Data.List((\\),sortBy)
import Data.Maybe(listToMaybe)
import Command
--import Direction
import Dlayout(invisibleGroupF)
import Event
--import Font(FontStruct)
import Fudget
import FRequest
--import Geometry(origin) --Line(..), Point(..), Rect(..), Size(..), origin)
--import LayoutRequest(LayoutRequest)
import LoopLow
import HbcUtils(union)
--import Message(Message(..))
import Path
import PathTree hiding (pos)
--import SP
import Spops
import CompSP
import Utils
import CmdLineEnv(argKey,argReadKey)
import WindowF(kernelTag,autumnize)
import Xtypes
--import List2(sort)

import Maptrace

getEventMask [] = Nothing
getEventMask (CWEventMask m : _) = Just m
getEventMask (_ : l) = getEventMask l

setEventMask em [] = [CWEventMask em]
setEventMask em (CWEventMask m : l) = CWEventMask em : l
setEventMask em (wa : l) = wa : setEventMask em l

focusBtn = Button 1
focusMods = [] --rotMods
rotMods = argReadKey "rotmods" [] :: ModState
rotKs = argKey "rotkey" "Tab" :: KeySym

entrymask = [KeyPressMask, EnterWindowMask, LeaveWindowMask]
mask = [KeyPressMask, KeyReleaseMask, EnterWindowMask, LeaveWindowMask]

mkFocusEvent io = io NotifyNonlinearVirtual NotifyNormal

focusMgr sizing ctt f = loopThroughLowF focusK0 igF
  where
  igF = invisibleGroupF sizing [] [CWEventMask mask] f
  focusK0 = prepostMapSP pre post 
	       (focusK' False emptyPathTree False [] mask [] [] []) where
      pre (Left (t,m)) = (t,Left m)
      pre (Right (t,m)) = (t,Right m)
      post (t,Left m) = Left (t,m)
      post (t,Right m) = Right (t,m)

  --focusK' :: PathTree Bool ->  Bool ->  [(Bool,Path)] -> [EventMask] -> [(Path,(XEvent -> Maybe XEvent))] -> [Path] -> [(Bool,Path)] -> SP (Path,Either Command Event) (Path,Either Command Event) 
  focusK' focusin mapped inw grab mask tt shellTags etags = same where
   focusK = focusK' focusin mapped
   focusm = focusK inw grab mask tt
   modMapped tag raised = changeFocusc' mapped' id 
         [(and (spineVals mapped' t),t) | (_,t) <- etags]
      where mapped' = updateNode id True mapped (autumnize tag) (const raised)
   changeFocusc' mapped' c netags = 
       inw && listToMaybe (mappedtags etags) /= listToMaybe (mappedtags netags)
         `thenC` (leaveFocus etags . enterFocus netags) $ 
       c $
       focusK' focusin mapped' inw grab mask tt shellTags netags 
   changeFocusc = changeFocusc' mapped
   changeFocus = changeFocusc id
   rotate ts = case break fst ts of
       (unmapped,[]) -> ts
       (unmapped,t:mapped) -> mapped ++ unmapped ++ [t]
   nexttag = rotate etags
   prevtag = reverse (rotate (reverse etags))
   enterFocus et = putFocus et FocusIn `ifC` inw
   leaveFocus et = putFocus et FocusOut `ifC` inw
   putFocus et f = putFocus' et (mkFocusEvent f)
   mappedtags et = [t |(True,t)<- et]
   putFocus' et ev = case mappedtags et of 
	 [] -> id
	 (t:_) -> putSP (t, Right (XEvt ev))
   same = getSP focushandle
   focushandle tmsg@(tag,msg) = case msg of 
      Left cmd ->
        case cmd of 
	  XCmd xcmd -> case xcmd of
	    GrabEvents t -> putSP (tag, Left (XCmd $ GrabEvents (t || stag))) $
			    stag `thenC` (leaveFocus etags .
					  enterFocus [(True,tag)]) $
			    focusK inw ((not stag,tag):grab) 
					   mask tt shellTags etags
	    UngrabEvents -> null grab' && inw `thenC` enterFocus etags $
			    pass $ focusK inw grab' mask tt shellTags etags
			  where grab' = drop 1 grab
	    TranslateEvent t tmask ->  putSP (kernelTag, 
	       Left (XCmd $ ChangeWindowAttributes [CWEventMask umask])) $
		  focusK inw grab umask ((tag,t):tt) shellTags etags 
	      where umask = union mask tmask
	    ChangeWindowAttributes cwa | ctt && not ktag && not stag ->
	       case getEventMask cwa of
		  Just em -> if issubset entrymask em then
				putSP (tag, Left (XCmd $ ChangeWindowAttributes 
				       (setEventMask ((ButtonPressMask:
					      em) \\ entrymask) cwa))) $
				null etags `thenC` enterFocus etags' $
				focusm shellTags etags'
			     else passame
			where etags' = sortBy (\(_,x) (_,y)-> compare x y)
					      ((False,tag):etags)
		  Nothing -> passame
	    DestroyWindow -> pass $ 
		 -- not (null etags) && not (keep (head etags)) `thenC`
	       enterFocus etags' $
	       focusK inw grab mask tt' shellTags' (etags' :: [(Bool,Path)])
	      where keep = not . subPath tag
		    etags' = filter (keep.snd) etags
		    shellTags' = filter keep shellTags
		    tt' = filter (keep.fst) tt
	    MapRaised   -> changeMapping tag True
	    UnmapWindow -> changeMapping tag False
	    _ -> passame
	  XReq (CreateMyWindow _) -> changeMapping tag False
	  XReq (CreateRootWindow _ _) {-  | not ktag -} ->
	     pass $ focusm (autumnize tag: shellTags) etags
	  XReq (CreateSimpleWindow rchild _) -> 
	      changeMapping (absPath (autumnize tag) rchild) False
	  _ -> passame
       where changeMapping tag raised = ctrace "focus1" (raised,tag) $ pass $ modMapped tag raised
      Right (XEvt ev) ->
        if stag then passame
	else case ev of
	   ButtonEvent {state=mods,type'=Pressed,button=bno} | ctt && mods == focusMods 
	      && bno == focusBtn && etag -> changeFocusc pass (aft++bef)
 	      where (bef,aft) = break (flip subPath tag.autumnize.snd) etags
	   _ -> case flookup ev tt of
	       Just (t,e) -> putSP (t,Right (XEvt e)) same
	       Nothing -> if not ctt then passame else 
	         case ev of
		  KeyEvent {state=mods,type'=Pressed,keySym=ks} | ks == rotKs && ktag ->
		    if (Shift:rotMods) `issubset` mods then changeFocus prevtag
		    else if rotMods    `issubset` mods then changeFocus nexttag
		    else passame
		  KeyEvent {} |tag==kernelTag || gtag' -> toFocus same
		  EnterNotify {detail=d,focus=True} | tag==kernelTag -> handleEL False d True
		  LeaveNotify {detail=d,focus=True} | tag==kernelTag -> handleEL False d False
		  FocusIn  {detail=d} | tag==kernelTag || gtag' -> handleEL True d True
		  FocusOut {detail=d} | tag==kernelTag || gtag' -> handleEL True d False
		  _ -> passame
	where toFocus = putFocus' etags ev 
	      handleEL isFocusEv d e = if d == NotifyInferior 
                                       || (not isFocusEv && focusin) -- focus events have priority over crossing events
                       then passame else
		       (case grab of
			  (my,t):_ -> if my then if ktag then focusEvToFocus
						 else pass
				      else if ktag then 
					     putSP (t,Right (XEvt ev))
					    else pass
			  [] -> if ktag then focusEvToFocus else pass ) $ 
		       focusK' focusin' mapped inw' grab mask tt shellTags etags
		  where inw' = if ktag then e else inw
                        focusin' = if ktag && isFocusEv then e else focusin
                        focusEvToFocus = putFocus etags (if e then FocusIn else FocusOut)
      Right _ -> passame
    where pass = putSP tmsg
	  passame = pass same
	  ktag = tag == kernelTag || gtag
	  stag = inGroup shellTags
	  etag = inGroup (map (autumnize.snd) etags)
	  gtag = case grab of (_,t):_ -> t == tag; [] -> False
	  gtag' = case grab of (True,t):_ -> t == tag; [] -> False -- event grabbed by something in my shell
          inGroup tags = any (flip subPath tag) tags

flookup index' [] = Nothing
flookup index' ((t, p) : table') =
    case p index' of
      Nothing -> flookup index' table'
      Just e -> Just (t, e)
