module PopupMenuF(popupMenuF,oldPopupMenuF,oldPopupMenuF') where
--import ButtonGroupF
import Command
import CompOps((>=^<), (>^=<),(>+<))--(>==<), 
import InfixOps((>=..<))--(>^^=<),
import Dlayout(groupF)
import Event
--import Font(FontStruct)
import Fudget
import FRequest
--import Geometry(Line, Point, Rect, Size(..))
import GreyBgF(changeBg)
--import LayoutRequest(LayoutRequest)
import MenuF(menuAltsF,toEqSnd,fstEqSnd,sndEqSnd)--EqSnd,
import MenuPopupF(PopupMenu(..))
import DynListF(dynF)
--import Message(Message(..))
import Path(here)
import SerCompF(serCompLeftToRightF)--idRightF,
import Spops
import EitherUtils(mapEither)
import Xtypes
import CompSP(serCompSP)
import Defaults(bgColor,menuFont)
import Utils(pair)
import NullF(delayF)
--import ShowCommandF(showCommandF) -- debugging
--import SpyF(teeF) -- debugging

--popupMenuF :: [(alt,String)] -> F i o -> F (Either x i) (Either alt o)
popupMenuF alts f =
    mapEither fstEqSnd id>^=<
    oldPopupMenuF bgColor True menuFont (Button 3) [] []
                  (pre alts) sndEqSnd f
    >=^< mapEither pre id
  where
    pre = map (`pair` []) . toEqSnd

oldPopupMenuF bgcolor grab fname button mods keys alts show_alt f = 
 serCompLeftToRightF $
 oldPopupMenuF' bgcolor grab fname button mods keys alts show_alt f

oldPopupMenuF' bgcolor grab fname button mods keys alts show_alt f =
    let grabeventmask = [ButtonPressMask, ButtonReleaseMask]
        grabcmd = if grab then [GrabButton True button mods grabeventmask]
	          else []
        eventmask =
	  (if null keys then [] else [KeyPressMask, KeyReleaseMask]) ++
          (if grab then [] else (OwnerGrabButtonMask:grabeventmask)) ++
	  [LeaveWindowMask]
        startcmds = grabcmd ++ [ChangeWindowAttributes [CWEventMask eventmask]]
        ungrab = concatMapSP un where
	       un (High m) = [High m,Low (here,XCmd UngrabEvents)]
	       un m = [m]

        F dynAltsFSP = dynAltsF
	dynAltsF =
	    dynF (altsF alts) >=^< mapEither altsF id
	  where
	    altsF alts' = delayF' (menuAltsF fname (map fst alts') show_alt)
	     -- !! keyboard shortcuts ignored !!
	    delayF' f = delayF f >=..< filterSP notDestroy
	    --delayF' = id
	    --delayF' f = delayF (showCommandF "altsF" f >==< teeF show "altsF: ")
	    notDestroy (_,XEvt (DestroyNotify _)) = False
	    notDestroy _ = True

    in  (groupF (map XCmd startcmds)
               (changeBg bgcolor (actionK grab button keys mods))
               (F{-ff-} (ungrab `serCompSP` dynAltsFSP) >+< f))

actionK grab button keys mods = K{-kk-} $ concmapSP action where
    toF = High . Right
    toMenu = High . Left . Right
    newMenu = High . Left . Left
    action msg = case msg of
      High (Right hmsg) -> [toF hmsg]
      High (Left alts) -> [newMenu alts] -- breaks backwards compatibility...
      Low (XEvt ev) -> case ev of
        ButtonEvent {rootPos=rootPos,state=m,type'=Pressed,button=b} | m == mods && b == button -> 
	       [Low $ XCmd (GrabEvents True),toMenu (PopupMenu rootPos ev)]
        KeyEvent {rootPos=rootPos,state=m,type'=Pressed,keySym=ks} | (m, ks) `elem` keys -> 
	       [toMenu (PopupMenu rootPos ev)]
        LeaveNotify {mode=NotifyUngrab} -> 
	       [Low $ XCmd UngrabEvents,toMenu PopdownMenu]
        ButtonEvent {type'=Released} -> 
	       [Low $ XCmd UngrabEvents,toMenu PopdownMenu]
        KeyEvent {type'=Released} -> [toMenu PopdownMenu]
        _ -> []
      Low _ -> []
