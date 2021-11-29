module ButtonGroupF(
	buttonGroupF, menuButtonGroupF,
	--buttonMachineF,
	BMevents(..))
where
import Command
import CompOps((>=^<))
import Defaults(bgColor)
import Dlayout(groupF)
import Event
import Fudget
import FRequest
import Xcommand
--import Geometry(Line, Point, Rect, Size(..))
import GreyBgF(changeBg)
--import LayoutRequest(LayoutRequest)
import Loops(loopLeftF)
import Message(message) --Message(..),
import NullF
--import SpEither(mapFilterSP)
import Xtypes
import Utils

data BMevents = BMNormal | BMInverted | BMClick  deriving (Eq, Ord, Show)

buttonGroupF     = buttonGroupF' cmdButton
menuButtonGroupF = buttonGroupF' menuButton []

buttonGroupF' bp keys f = loopLeftF (buttonMachineF' bp keys f >=^< Right)

--buttonMachineF = buttonMachineF' cmdButton

buttonMachineF' bp keys = groupF [] (changeBg bgColor (buttonK bp keys))

data ButtonParams =
  BP { modstate :: ModState,
       mbutton :: Button,
       bmachine :: Button -> ModState -> K Bool BMevents }

elbMask = [EnterWindowMask, LeaveWindowMask, ButtonPressMask, ButtonReleaseMask]

cmdButton =
  BP { modstate = [],
       mbutton = Button 1,
       bmachine = buttonMachine }

menuButton =
  BP { modstate = [],
       mbutton = Button 1, -- not used
       bmachine = mbuttonMachine }

buttonMachine mousebutton modstate =
    setEventMask [] $
    xcommandK (GrabButton False mousebutton modstate grabbedMask) $
    bm BMNormal
  where
    grabbedMask = elbMask
    switch newme = putK (High newme) (bm newme)
    pressed = switch BMInverted
    normal = switch BMNormal
    clicked = putsK [High BMNormal, High BMClick] (bm BMNormal)
    changeMode =
	-- switch to menu button mode
	xcommandK (UngrabButton mousebutton modstate) $
	mbuttonMachine mousebutton modstate
    bm me =
      let nochange = bm me
      in getK $ \msg ->
	 case msg of
	   Low (XEvt event) ->
	     case event of
	       (EnterNotify {detail=d}) | d /= NotifyInferior -> pressed
	       (LeaveNotify {detail=d}) | d /= NotifyInferior -> normal
	       (ButtonEvent {state=s,type'=Pressed,button=b})
	         | b == mousebutton && modstate `issubset` s -> pressed
	       (ButtonEvent {type'=Released,button=b})
	         | b == mousebutton -> if me == BMInverted
	                               then clicked
				       else nochange
	       (MenuPopupMode True) -> changeMode
	       _ -> nochange
	   High True -> changeMode
	   _ -> nochange

mbuttonMachine mousebutton modstate =
    setEventMask elbMask $
    loop
  where
    loop = getK $ message low high
    out e = putK (High e) loop
    normal = out BMNormal
    pressed = out BMInverted
    clicked = out BMClick

    low (XEvt ev) = event ev
    low _ = loop
    event (ButtonEvent {type'=Released}) = clicked
    event (EnterNotify {}) = pressed
    event (LeaveNotify {}) = normal
    event (MenuPopupMode False) = buttonMachine mousebutton modstate
    event _ = loop

    high False = buttonMachine mousebutton modstate
    high _ = loop

buttonK :: ButtonParams -> [(ModState, KeySym)] -> K Bool BMevents
buttonK (BP {mbutton=mbutton, modstate=modstate, bmachine=bmachine }) keys =
    xcommandsK initcmds $ bmachine mbutton modstate
  where
    initcmds = transinit ++ [MeButtonMachine]

    transinit =
	if null keys
	then []
	else [TranslateEvent tobutton [KeyPressMask, KeyReleaseMask]]

    tobutton (KeyEvent t p1 p2 s pressed _ ks _) | (s, ks) `elem` keys =
	Just (ButtonEvent t p1 p2 modstate pressed mbutton)
    tobutton _ = Nothing

setEventMask mask = xcommandK (ChangeWindowAttributes [CWEventMask mask])
