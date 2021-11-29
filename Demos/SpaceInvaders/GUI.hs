module GUI(Draw(..),HasSize(..),PlacedPict,pp,clear,clearpm,clearRect,wDraw,
           Pict,GCId,objGC,readPict,rectangles,text,
           overlaps,diffRect,message,Move(..),Rect(..),Point(..),pP,lL,Drawing,
           line2rect,inRect,rR,hboxD,atomicD,Click,Tick,
           argReadKey,reactiveSP,
           putLivesMkc,G,putGUI,putsGUI,putLives,putScore,getG,spaceF,--nullG,
           shotColor, shelterColor, ufoColor, readObjPict,readObjPicts
           ) where
import Control.Applicative
import AllFudgets hiding (draw,wDraw,size,state,rect,put)
import ReactiveF
import ReadPic
import InvaderTypes

{-
type G i o = K i o

nullG = nullK
putLives l = putK (High (Left l))
putScore s = putK (High (Right s))
putGUI gfx = putK (Low gfx)
putsGUI gfx = putsK (map Low gfx)
-}
type G i o = KSP i o

--nullG = nullSP
putLivesMkc l = toMkc $ putSP (High (Left l))
putLives l = put (High (Left l))
putScore s = put (High (Right s))
putGUI gfx = put (Low gfx)
putsGUI gfx = mapM_ (put . Low) gfx

spaceF space initK playK =
    windowF startcmds (changeBg "black" (K playKSP)) >==<idRightF tickF
  where
    playKSP = unMk initK (\ w0 ->playK w0 `serCompSP` getGSP)
    
    startcmds = [layoutRequestCmd (plainLayout space True True),
                 XCmd $ ChangeWindowAttributes [CWEventMask eventmask]]
    eventmask = [KeyPressMask,KeyReleaseMask,
                 ButtonPressMask,ButtonReleaseMask,
		 LeaveWindowMask,EnterWindowMask,ExposureMask]

    tickF = startupF [False] (timerF>=^<pre)
      where
        pre True = Nothing
	pre False = Just (dt,dt)

dt = argReadKey "dt" 20::Int

--------------------------------------------------------------------------------

getGSP = getG $ flip putSP getGSP

getG return = loop
  where
    loop = getSP (message low high)

    high = return . High

    retLow = return . Low

    low (XEvt ev) = event ev
    low _ = loop

    event ev =
      case ev of
        Expose r 0     -> retLow Redraw
        ButtonEvent {} -> btn (type' ev) (button ev)
        KeyEvent    {} -> key (type' ev) (keySym ev)
        _              -> loop

    btn Pressed (Button 2) = retLow (Key Fire Down)
    btn pressed (Button 1) = updown pressed MoveLeft
    btn pressed (Button 3) = updown pressed MoveRight
    btn _       _          = loop

    key Pressed "space"   = retLow (Key Fire Down)
    key pressed "Shift_L" = updown pressed MoveLeft
    key pressed "Shift_R" = updown pressed MoveRight
    key pressed "Left"    = updown pressed MoveLeft
    key pressed "Right"   = updown pressed MoveRight
--  key Pressed _         = putGUI bell loop
    key _ _               = loop

    updown Pressed  a = retLow (Key a Down)
    updown Released a = retLow (Key a Up)
    updown _        _ = loop

--------------------------------------------------------------------------------

data PlacedPict = PP Pict Point
type Pict = FixedColorDrawing

pp = PP
drawPP (PP (FixCD s gcdcmds) p) = [(gc,move p dcmds)|(gc,dcmds)<-gcdcmds]
drawIt obj = concatMap drawPP (drawing obj)
wDraw obj = wDrawMany (drawIt obj)

class Draw obj where drawing :: obj -> [PlacedPict]

instance Draw obj => Draw [obj] where drawing = concatMap drawing

instance (Draw o1,Draw o2) => Draw (o1,o2) where
  drawing (o1,o2) = drawing o1++drawing o2

--------------------------------------------------------------------------------
unMkc op ksp = ksp'
  where K ksp' = unMk op (\c->K (ksp c))

objGC fg = Mk $ unMkc (objGC' fg)

objGC' fg =
  do ~(Just bgpixel) <- Mk (tryConvColorK "black")
     ~(Just fgpixel) <- Mk (tryConvColorK [fg,"white"])
     let attrs = [GCBackground bgpixel,GCForeground fgpixel]
     Mk (wCreateGC rootGC (GCGraphicsExposures False:attrs))


readObjPicts (obj1,obj2)  = (,) <$> readObjPict obj1 <*> readObjPict obj2

readObjPict obj = flip readPict obj =<< objGC (objectColor obj)

readPict gc path = pmPict gc <$> readPic' path

readPic' path = Mk $ unMkc (readPic path)

pmPict gc pm@(PixmapImage size _) = FixCD size (copy gc pm 0)

rectangles gc s rs = FixCD s [(gc,map FillRectangle rs)]

text gc s = FixCD (pP (6*length s) 13) [(gc,[DrawString 0 s])] -- !!

clearpm p pict = clear p (size pict)

clear p size = clearRect (Rect p size) False

clearRect r e = XCmd $ ClearArea r e

copy gc pm p = [(gc,[copy' pm p])]
copy' (PixmapImage size pm) p = CopyPlane (Pixmap pm) (Rect origin size) p 0

bell = XCmd (Bell 0)

--------------------------------------------------------------------------------

class HasSize a where size :: a -> Size

instance HasSize PixmapImage where size (PixmapImage s pm) = s
instance HasSize FixedColorDrawing where size (FixCD s _) = s
instance HasSize FixedDrawing where size (FixD s _) = s


--------------------------------------------------------------------------------

objectColor obj =
  case obj of
    Vader r    _ -> vaderColor r
    VExplode r   -> vaderColor r
    VShot      _ -> vshotColor
    Base         -> baseColor
    Explode      -> baseColor
    Ufo          -> ufoColor

baseColor    = argKey "baseColor"    "cyan"
shotColor    = argKey "shotColor"    "lavender"
vshotColor   = argKey "vshotColor"   "orange"
shelterColor = argKey "shelterColor" "yellow"
ufoColor     = argKey "ufoColor"     "grey"

vaderColor r = argKey ("vader"++show (rowNum r)++"Color")  (rowColor r)

rowColor r = case r of R1-> "blue"; R2 -> "green"; R3 -> "red"
