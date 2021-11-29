module DrawingF where

import AllFudgets
import MyUtils

{-
drawDisplayF size drawing =
  let startcmds = [ ConfigureWindow [CWBorderWidth 0],
		    layoutRequestCmd (plainLayout size True True),
		    ChangeWindowAttributes [ CWEventMask [ExposureMask]
					     --,CWBackingStore WhenMapped
					   ]
		  ]
  in windowF startcmds (drawDisplayK drawing)

drawDisplayK drawing =
  wCreateGC rootGC [GCFunction GXcopy] (\gc -> drawDisplayP gc drawing)

drawDisplayP gc drawing0 =
  let draw drawing = map (Low . Draw MyWindow gc) drawing
  in let step drawing msg =
	   case msg of
		Low (Expose _ 0) -> (drawing, draw drawing)
		High drawing'   -> (drawing',Low ClearWindow:(draw drawing'))
		_		 -> (drawing,[])
  in putsK (draw drawing0) $ mapstateSP step drawing0
-}
------

drawDisplayCF size drawing =
  let startcmds =  [ XCmd $ ConfigureWindow [CWBorderWidth 0],
		    layoutRequestCmd (plainLayout size True True),
		     XCmd $ ChangeWindowAttributes [ CWEventMask [ExposureMask
							,ButtonPressMask]
					     --,CWBackingStore WhenMapped
					   ]
		  ]
  in windowF startcmds (drawDisplayCK drawing)

drawDisplayCK drawing =
  wCreateGC rootGC [GCFunction GXcopy] (\gc -> drawDisplayCP gc drawing)

drawDisplayCP gc drawing0 =
  let draw drawing = map (Low . wDraw gc) drawing
  in let step drawing msg =
	   case msg of
		Low (XEvt (Expose _ 0)) -> (drawing, draw drawing)
		High drawingf'   -> let drawing' = drawingf' drawing
				    in (drawing',Low (XCmd ClearWindow):(draw drawing'))
		Low (XEvt (ButtonEvent t x y m Pressed b)) -> 
		    (drawing,High (ButtonEvent t x y m Pressed b):(draw drawing))
		_		 -> (drawing,[])
  in putsK (draw drawing0) $ K $ mapstateSP step drawing0

------

drawDisplayCKF size drawing =
  let startcmds = [ XCmd $ ConfigureWindow [CWBorderWidth 0],
		    layoutRequestCmd (plainLayout size True True),
		    XCmd $ ChangeWindowAttributes [ CWEventMask [ExposureMask
							,ButtonPressMask]
					     --,CWBackingStore WhenMapped
					   ]
		  ]
  in windowF startcmds (drawDisplayCKK drawing)

drawDisplayCKK drawing =
  wCreateGC rootGC [GCFunction GXcopy] (\gc -> drawDisplayCKP gc drawing)

drawDisplayCKP gc drawing0 =
  let draw drawing = map (Low . wDraw gc) (drawing0++drawing)
  in let step drawing msg =
	   case msg of
		Low (XEvt (Expose _ 0)) -> (drawing, draw drawing)
		High drawingf'   -> let drawing' = drawingf' drawing
				    in (drawing',Low (XCmd ClearWindow):(draw drawing'))
		Low (XEvt (ButtonEvent t x y m Pressed b)) -> 
		    (drawing,High (ButtonEvent t x y m Pressed b):(draw drawing))
		_		 -> (drawing,[])
  in putsK (draw []) $ K $ mapstateSP step drawing0

------

drawDisplayDF size drawing =
  let startcmds = [ XCmd $ ConfigureWindow [CWBorderWidth 0],
		    layoutRequestCmd (plainLayout size True True),
		    XCmd $ ChangeWindowAttributes [ CWEventMask [ExposureMask
							,ButtonPressMask]
					     --,CWBackingStore WhenMapped
					   ]
		  ]
  in windowF startcmds (drawDisplayDK drawing)

drawDisplayDK drawing =
  wCreateGC rootGC [GCFunction GXcopy] (\gc -> drawDisplayDP gc drawing)

drawDisplayDP gc drawing0 =
  let draw drawing = map (Low . wDraw gc) drawing
  in let step drawing msg =
	   case msg of
		Low (XEvt (Expose _ 0)) -> (drawing, draw drawing)
		High drawingf'   -> let drawing' = drawingf' drawing
				    in (drawing',Low (XCmd ClearWindow):(draw drawing'))
		Low (XEvt (ButtonEvent _ _ _ _ Pressed _)) -> 
		    (drawing,High (drawing):(draw drawing))
		_		 -> (drawing,[])
  in putsK (draw drawing0) $ K $ mapstateSP step drawing0
