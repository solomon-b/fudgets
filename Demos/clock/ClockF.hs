{-# LANGUAGE CPP #-}
module ClockF(clockShellF) where
import AllFudgets
--import DialogueIO hiding (IOError)
import Data.Maybe(fromMaybe)
#ifdef VERSION_old_time
import System.Time(CalendarTime(..))
#else
import Data.Time(TimeOfDay(..),localTimeOfDay,zonedTimeToLocalTime)
#endif

--default(Int)

trace x y = y
--ctr s x = ctrace s (show x) x

data ClockMode = Analog | Digital deriving (Show,Eq)

clockShellF =
  let size = fsize
      gcattrs = [GCFont digFont,
		 GCLineWidth width,
		 GCCapStyle CapRound]
      clockShellK = safeLoadQueryFont digFont (\font ->
		       dynShapeK gcattrs (const [])
				 (clockK startMode (digsize font)
			                 (Point 0 (font_ascent font))
			                 size))
      menuF = menuAltsF menuFont [Analog,Digital] show -- >=^<Left
      tickF = (Left. Right. Right)>^=<startupF [Just (update,1000)] timerF
      startcmds = map (Low . XCmd) [StoreName "ClockFudget"
                   --,ConfigureWindow [CWBorderWidth 5] -- testing
		  ]
      route msg =
	 case msg of
	   Left (Right (Left shape)) -> Left (Left shape)
	   Left (Right (Right pop)) -> Right pop
	   Right mode -> Left (Right (Left mode))
  in loopOnlyF (route>^=<shellKF (putsK startcmds clockShellK) menuF)>==<tickF

clockK :: ClockMode -> Size -> Point -> Size -> K (Either ClockMode Tick) (Either (Point->[DrawCommand]) PopupMenu)
clockK dig digsize bp size =
  let clockP dig size =
        let redraw s t = putsK [--Low (ConfigureWindow [CWBorderWidth 5]), -- testing
	                       High (Left (clockface dig bp t))] (clockP dig s)
            --gettime dig s = putLow (DReq GetLocalTime) (clockP dig s)
#ifdef VERSION_old_time
            gettime dig s = getLocalTime (redraw s)
#else
            gettime dig s = getZonedTime (redraw s)
#endif
            same = clockP dig size
        in getK $ \msg ->
	   case msg of
	     --Low (Expose _ 0)-> gettime dig size -- ??
	     --Low (DResp (Dbl t))-> redraw size t
	     Low (XEvt ev@(ButtonEvent _ _ rootpos _ Pressed _)) ->
	       putK (High (Right (PopupMenu rootpos ev))) same
	     Low (XEvt (ButtonEvent _ _ _ _ Released _))->
	       putK (High (Right PopdownMenu)) same
	     High (Right _) -> gettime dig size
	     High (Left mode) -> if mode==dig
			        then same
				else putK (layout mode) (gettime mode size)
	     _-> same

      layout mode =
        let s = case mode of
	          Digital-> digsize
		  Analog-> size
	in Low (layoutRequestCmd (plainLayout s False False))
      layout0 = layout dig
      startcmds =
        [ layout0,
          Low (XCmd $ GrabButton True AnyButton [] [ButtonPressMask,ButtonReleaseMask])
	]
  in changeBg color (putsK startcmds (clockP dig size))

#ifdef VERSION_old_time
timeofday CalendarTime{ctHour=h,ctMin=min,ctSec=s} =
  let t = s+60*(min+60*h)
      m = itof t / 60.0
  in (fmod (m/60.0) 24.0 ,fmod m 60.0)
#else
timeofday zt = (fmod (m/60.0) 24.0 ,fmod m 60.0)
  where
    t = round s+60*(min+60*h)
    m = itof t / 60
    TimeOfDay h min s = localTimeOfDay (zonedTimeToLocalTime zt)
#endif

clockface mode bp t size =
    case mode of
      Digital-> [DrawString bp (digits t)]
      Analog-> hands t size ++ scalemarks size

digits t =
  let (h,m) = aboth floor' (timeofday t)
  in show h ++ ":" ++ drop 1 (show (100+m))

hands t size =
  let (h,m) = timeofday t
  in hand h 12 0.6 size ++ hand m 60 0.95 size

hand v d r size =
  let a = v / itof d * 2.0 * pi
      h = scalePoint 0.5 size
  in [DrawLine (Line h (toRect r h a))]

scalemarks size =
  let r = scalePoint 0.5 size
      w2 = width*2
  in let mark n =
	  let a = itof n*pi/6.0
	  in Rect (psub (toRect 0.9 r a) (Point width width)) (Point w2 w2)
  in [FillArc (mark n) 0 (64*360) | n<-[0..11]]

toRect k (Point w h) r = Point (w+floor' (k*itof w*sin r)) (h+floor' (k*itof (-h)*cos r))

fmod :: Double -> Double -> Double
fmod a b =
  let y = a - b * fromIntegral (floor' (a/b))
  in {-if y<0.0 && argFlag "floor" False 
     then trace (show (a,b,y)) y
     else -}y

digsize font = string_box_size font "88:88"

width = argReadKey "width" 10
fsize = fromMaybe (diag (argReadKey "size" 200)) defaultSize
color = argKey "color" "gold"
update = (argReadKey "update" 20) * 1000
startMode = if argKey "mode" "analog" == "digital" then Digital else Analog
digFont = defaultFont

itof=fromIntegral

rightadj s n pad = [pad|i<-[length s+1..n]]++s

floor' x = floor x
-- Solves a problem on slip-02 with direct calls to PfloorDouble2Int. TH 970314.
