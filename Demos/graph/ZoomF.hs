module ZoomF(ZoomOutput(..),zoomF) where
import AllFudgets

data ZoomOutput
  = ZoomIn Size Point
  | ZoomOut Size Point
  | ZoomRect Size Rect
  | ZoomPick Size Point 
  | ZoomResize Size
  deriving (Eq,Show)

zoomF fud = groupF startcmds zoomK0 fud
  where
    startcmds = map XCmd $ 
                 [ConfigureWindow [CWBorderWidth 0],
                 ChangeWindowAttributes [CWEventMask eventmask],
		 clickGrab 1,
		 clickGrab 3,
		 GrabButton False (Button 2) []
				 [ButtonPressMask,
				 PointerMotionMask,
				 ButtonReleaseMask]]
    clickGrab n = GrabButton False (Button n) [] []
    eventmask = [ButtonPressMask]

    zoomK0 =
	setFontCursor 130 $
        allocNamedColorPixel defaultColormap bgColor $ \ bg ->
        allocNamedColorPixel defaultColormap fgColor $ \ fg ->
        wCreateGC rootGC (GCSubwindowMode IncludeInferiors:
	                  GCFunction GXxor:
			  invertColorGCattrs fg bg) $ \ gcinv ->
        waitForMsg layoutsize $ \ size ->
        zoomK gcinv size
      where
        layoutsize (Low (LEvt (LayoutSize size))) = Just size
	layoutsize _ = Nothing

    zoomK gcinv size =
        getK $ message low high
      where
        same = zoomK gcinv size
	high _ = same
	low event =
	  case event of
	    XEvt ButtonEvent {pos=p,state=mods} | Shift `elem` mods ->
		  put (ZoomPick size p) same
	    XEvt ButtonEvent {pos=p,button=Button 1} -> put (ZoomIn size p) same
	    XEvt ButtonEvent {pos=p,button=Button 3} -> put (ZoomOut size p) same
	    XEvt ButtonEvent {pos=p,button=Button 2,type'=Pressed} ->
	      putLow (wDrawRectangle gcinv (Rect p (Point 1 1))) $
	      rubberBandK gcinv size p p $ \ size' ->
	      zoomK gcinv size'
	    --XEvt MotionNotify {} ->
	    LEvt (LayoutSize size') ->
	      put (ZoomResize size') $
	      zoomK gcinv size'
	    _ -> same

rubberBandK gcinv size p0 p1 return =
     getK $ message low high
  where
    cont p = rubberBandK gcinv size p0 p return
    resize size' = rubberBandK gcinv size' p0 p1 return
    same = cont p1
    toggle p0 p1 = putLow $ wDrawRectangle gcinv (rect' p0 p1)
    high _ = same
    low event =
      case event of
        XEvt MotionNotify {pos=p} ->
	  toggle p0 p1 $
	  toggle p0 p $
	  cont p
	XEvt ButtonEvent {pos=p,type'=Released,button=Button 2} ->
	  toggle p0 p1 $
	  put (ZoomRect size (rect' p0 p1)) $
	  return size
        LEvt (LayoutSize size') -> resize size'
	_ -> same

rect' p1 p2 =
  let p0 = pmin p1 p2
      p = pmax p1 p2
  in Rect p0 (p-p0+1)
