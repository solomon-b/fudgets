module ButtonBorderF(buttonBorderF) where
--import BgF
import Border3dF
--import ButtonGroupF
import Color
import Command(XCommand(ChangeWindowAttributes,ClearArea,DrawMany,Draw))
import XDraw
import CompOps((>^=<))
import Defaults(look3d, shadowColor, shineColor,bgColor)
import Dlayout(groupF)
import Event
import Fudget
--import FudgetIO
import FRequest
import Xcommand
import Gc
import Geometry(Line(..), Point(..), Rect(..), origin, pP, padd, psub)
import LayoutRequest
--import Message(Message(..))
import NullF
import Spacer(marginF)
import EitherUtils(stripEither)
import Xtypes
import GreyBgF(changeBg)

buttonBorderF :: Int -> F a b -> F (Either Bool a) b
buttonBorderF = if look3d then border3dF False else stdButtonBorderF

stdButtonBorderF edgew f =
    let kernel =
          changeBg bgColor $
	  allocNamedColorDefPixel defaultColormap shineColor "white" $ \shine->
	  allocNamedColorDefPixel defaultColormap shadowColor "black" $ \shadow ->
	  wCreateGC rootGC [GCFunction GXcopy, GCForeground shadow,
			    GCBackground shine] $ \drawGC ->
	  wCreateGC drawGC [GCForeground shine] $ \shineGC ->
	  wCreateGC rootGC (invertColorGCattrs shine shadow) $ \invertGC ->
	  let bpx = edgew
	      bpy = edgew
	      upperLeftCorner = Point bpx bpy
	      dRAW s = 
		 let size@(Point sx sy) = psub s (Point 1 1)
		     rect = Rect origin size
		     upperRightCorner = Point (sx - bpx) bpy
		     lowerLeftCorner = Point bpx (sy - bpy)
		     lowerRightCorner = psub size upperLeftCorner
		     leftBorder = Line upperLeftCorner lowerLeftCorner
		     upperBorder = Line upperLeftCorner upperRightCorner
		     upperLeftLine = Line origin upperLeftCorner
		     lowerRightLine = Line lowerRightCorner size
		     incx = padd (Point 1 0)
		     incy = padd (Point 0 1)
		     decx = padd (Point (-1) 0)
		     decy = padd (Point 0 (-1))
		     lowerBorderPoints = [lowerLeftCorner, lowerRightCorner, 
					  upperRightCorner, Point sx 0, size, Point 0 sy]
		     borderPoints =
		       [pP 1 1, pP 1 sy, size, pP sx 1, origin, upperLeftCorner, 
		        incy lowerLeftCorner, (incx . incy) lowerRightCorner, 
			incx upperRightCorner, upperLeftCorner]
		 in  ( [ClearArea rect False, 
		        DrawMany MyWindow [
			  (shineGC,[FillPolygon Nonconvex CoordModeOrigin
			            borderPoints]),
			  (drawGC,[FillPolygon Nonconvex CoordModeOrigin 
			           lowerBorderPoints, 
			           DrawLine leftBorder, 
			           DrawLine upperBorder, 
			           DrawLine upperLeftLine]), 
			  (invertGC,[DrawLine lowerRightLine]), 
			  (drawGC,[DrawRectangle rect])]], 
			 [Draw MyWindow invertGC $ FillPolygon Nonconvex 
				  CoordModeOrigin borderPoints])
	      proc pressed size =
		  getK $ \bmsg ->
		  let same = proc pressed size
		      (drawit_size, pressit_size) = dRAW size
		      redraw b = if b == pressed then [] else pressit_size
		  in  case bmsg of
			Low (XEvt (Expose _ 0)) -> xcommandsK (drawit_size ++ 
			    (if pressed then pressit_size else [])) same
			Low (LEvt (LayoutSize newsize)) -> proc pressed newsize
			High change -> xcommandsK (redraw change) (proc change size)
			_ -> same
	      proc0 pressed =
		  getK $ \msg ->
		  case msg of
		    Low (LEvt (LayoutSize size)) -> proc pressed size
		    High change -> proc0 change
		    _ -> proc0 pressed
	  in  proc0 False

        startcmds = [XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask]]]
    in  stripEither >^=< ((groupF startcmds kernel . marginF (edgew + 1)) f)

