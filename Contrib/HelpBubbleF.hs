module HelpBubbleF(helpBubbleF) where
import AllFudgets

data BubbleState = Idle | Armed | Up

helpBubbleF help fud =
    if useBubbles
    then loopCompThroughLeftF $
         groupF startcmds ctrlK0 ((timerF>+<bubbleF) >+< fud)
    else fud
 where
   bubbleF = bubbleRootPopupF (labelF' lblpm help)
   lblpm = setBgColor "white" . setFont helpFont
   eventmask = [EnterWindowMask,LeaveWindowMask]
   startcmds = [XCmd $ ChangeWindowAttributes [CWEventMask eventmask],
                XCmd $ ConfigureWindow [CWBorderWidth 0]]

   ctrlK0 = ctrlK 0 0 Idle

   toTimer = High . Left
   toBubble = High . Right

   ctrlK size pos bubbleState =
      getK $ message event (either fromTimer fromBubble)
     where
       same = ctrlK size pos bubbleState
       idle = ctrlK size pos Idle
       newSize size' = ctrlK size' pos bubbleState
       timerOff s = putK (toTimer Nothing) $ ctrlK size pos s
       timerOn pos' = putK (toTimer (Just (0,500))) $ ctrlK size pos' Armed

       fromBubble _ = same

       fromTimer Tick =
         case bubbleState of
	   Armed ->
	       putK (toBubble (Popup (pos+offset) ())) $
	       timerOff Up
	     where offset = pP (xcoord size `div` 2) 3
	   _ -> same
       event e =
         --echoK (show e) $
         case e of
	   XEvt EnterNotify { pos=pos,rootPos=rootPos } -> timerOn (rootPos-pos)

	   XEvt LeaveNotify { } ->
	     case bubbleState of
	       Idle -> same
	       Armed -> timerOff Idle
	       Up -> putK (toBubble Popdown) $ idle

	   LEvt (LayoutSize size') -> newSize size'
	   _ -> same

useBubbles = argFlag "helpbubbles" True
helpFont = argKey "helpfont" "-*-new century schoolbook-medium-r-*-*-12-*-*-*-*-*-iso8859-1"
