module Box(boxF,readAllBitmaps) where
import AllFudgets
import HbcUtils(lookupWithDefault)
import MineField
import Pics

pictSize :: Size
pictSize = Point 32 32

initcmds = [XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask]], but 1, but 2, but 3] 
           where but t = XCmd $ GrabButton True (Button t) [] [ButtonPressMask, ButtonReleaseMask]

boxF :: [(Symbol,PixmapId)] -> F Symbol (Coord -> MInput)
boxF sps = windowF initcmds (simpleK (boxK sps) pictSize SBlank)

boxK :: [(Symbol,PixmapId)] -> Drawer -> Drawer -> Fms' Symbol Symbol (Coord -> MInput)
boxK ps draw _clear s event =
    case event of
      High s'                                              -> (s', redraw ps s')
      Low (XEvt (Expose _ 0))                              -> (s, redraw ps s)
      Low (XEvt (ButtonEvent _ _ _ ms Pressed (Button b))) ->
          (s, case (ms,b) of
                ([]       ,1) -> [High Move]
                ([Control],1) -> [High Bomb]
                ([Shift]  ,1) -> [High Free]
                (_        ,2) -> [High Bomb]
                (_        ,3) -> [High Free]
                _             -> [])
--    Low (LayoutSize nsize)                     -> (s, ??)
      _ -> (s,[])
    where redraw ps SBlank = [Low $ XCmd ClearWindow]
          redraw ps s = 
		let p = lookupWithDefault ps (error ("No bitmap for "++show s)) s
		in  [Low $ XCmd ClearWindow, Low (draw (CopyPlane (Pixmap p) (Rect 0 pictSize) 0 0))]
{-
          redraw ps (SNumber 0) = [Low ClearWindow, Low (draw (CopyPlane (Pixmap ps) (Rect (Point 0 0) (Point 32 32)) (Point 0 0) 0))]
          redraw ps (SNumber n) = [Low ClearWindow, Low (draw (DrawImageString tp (show n)))]
          redraw ps (SCurrent n) = [Low ClearWindow, Low (draw (DrawImageString tp ("X"++show n)))]
	  redraw ps SBombX = [Low ClearWindow, Low (draw (DrawImageString tp "BX"))]
	  redraw ps SBombDone = [Low ClearWindow, Low (draw (DrawImageString tp "BD"))]
	  redraw ps SBomb = [Low ClearWindow, Low (draw (DrawImageString tp "B"))]
	  redraw ps SFree = [Low ClearWindow, Low (draw (DrawImageString tp "F"))]
tp = Point 10 18
-}

bitmaps = 
    [(SNumber n,num!!n) | n<-[0..5]] ++
    [(SCurrent n,cur!!n) | n<-[0..5]] ++
    [(SBombX, bombx), (SBombDone, bombdone), (SBomb, bomb), (SFree, free)]

readAllBitmaps f = readBitmaps bitmaps f

--readBitmaps :: [(Symbol,String)] -> ([PixmapId] -> K a b) -> K a b
readBitmaps ns f = readb [] ns f
	where readb ps [] f = f ps
              readb ps ((n,bm):ns) f = bitmapFromData bm $ \r ->
	                          case r of
				    BitmapBad -> error ("Cannot read bitmap "++show n)
				    BitmapReturn _ _ p -> readb (ps++[(n,p)]) ns f
