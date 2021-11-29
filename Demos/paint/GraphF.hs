module GraphF(graphF,PaintCommand(..),ToolFunc(..)) where
import AllFudgets
--import Word()

data PaintCommand
	= ChangeTool ToolFunc
	| ChangeColor ColorName

type ToolFunc = Point->Point->DrawCommand

graphK size func bgcol gc gcinv =
	let same = graphK size func bgcol gc gcinv
	    newf f = graphK size f bgcol gc gcinv
	    resize s = graphK s func bgcol gc gcinv
	    newgc = graphK size func bgcol
	in getK $ \msg ->
	     case msg of
	        High (ChangeTool func) -> newf func
	        High (ChangeColor color) -> changeColorK bgcol color newgc
	        Low (LEvt (LayoutSize newsize)) -> resize newsize
	        Low (XEvt (Expose _ 0)) -> same -- !!!
	        Low (XEvt (ButtonEvent _ p _ _ Pressed (Button _))) ->
		       rubberBandK size func bgcol gc gcinv p p
	        _  -> same

rubberBandK s f bgcol gc gcinv p0 p1 =
	let cont p = rubberBandK s f bgcol gc gcinv p0 p
	in let same = cont p1
               draw gc p0 p1 = Low (wDraw gc (f p0 p1))
        in let toggle = draw gcinv
	in putK (toggle p0 p1) $
	   getK $ \msg ->
	     case msg of
	        Low (XEvt (MotionNotify _ p _ _)) -> putK (toggle p0 p1) (cont p)
	        Low (XEvt (ButtonEvent _ p _ _ Released _)) ->
		  putsK [toggle p0 p1, draw gc p0 p] (graphK s f bgcol gc gcinv)
	        _ -> same

idleGraphK size bgcol gc gcinv =
	let same = idleGraphK size bgcol gc gcinv
	    resize size' = idleGraphK size' bgcol gc gcinv 
	in getK $ \msg ->
	     case msg of
	        High (ChangeTool func) -> graphK size func bgcol gc gcinv
	        Low (LEvt (LayoutSize size')) -> resize size'
	        _ -> same

changeColorK bgcol fg k =
  let gcattr fgcol = [GCForeground fgcol, GCLineWidth 3]
      invattrs bg fg = if bg==fg
	               then invertColorGCattrs (Pixel 0) (Pixel 170) -- hmm
		       else invertColorGCattrs bg fg
  in allocNamedColorPixel defaultColormap fg  $ \fgcol ->
     wCreateGC rootGC (gcattr fgcol) $ \gc ->
     wCreateGC gc (invattrs bgcol fgcol) $ \gcinv ->
     k gc gcinv

graphF =
  let evmask =[ExposureMask{-,ButtonReleaseMask-}]
  in let wattrs = [CWBackingStore WhenMapped,
		CWEventMask evmask,
		CWBitGravity NorthWestGravity
		]
  in let startcmds = map XCmd 
                      [ ChangeWindowAttributes wattrs,
			--ConfigureWindow [CWBorderWidth 0],
			GrabButton True (Button 1) []
				[ButtonPressMask,
				PointerMotionMask,
				ButtonReleaseMask]
			{-,
			GrabButton True (Button 1) []
				[ButtonReleaseMask],
			GrabButton True (Button 3) []
				[ButtonReleaseMask]
			-}
		     ]
         size = Point 300 300
  in windowF startcmds $
	setFontCursor 130 $
	changeGetBackPixel "white" $ \bgcol ->
	changeColorK bgcol "black" $ \gc gcinv ->
	putK (Low (layoutRequestCmd (plainLayout size False False))) $
	idleGraphK size bgcol gc gcinv
