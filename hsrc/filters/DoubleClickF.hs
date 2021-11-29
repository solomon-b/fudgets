module DoubleClickF where
import Fudget
import FRequest
import Xtypes
import Event
--import Font(FontStruct)
import Spops
--import Geometry(Point,Rect,Size(..))
import CompOps((>=..<))

doubleClickF :: Time -> F a b -> F a b
doubleClickF timeout fudget = fudget >=..< doubleClickSP 0 dummy
  where
    dummy = error "bug in DoubleClickF.hs"
    doubleClickSP n last =
      getSP $ \ tev@(path,ev) ->
      case ev of
        XEvt (ButtonEvent t wp rp mods press btn) ->
	    if n==0 || press/=Pressed || t>lasttime+timeout ||
	       btn/=lastbtn || mods/=lastmods || path/=lastpath
	    then putSP tev $ doubleClickSP 1 tev
	    else let n' = n+1
		     tev' = (path,XEvt (ButtonEvent t wp rp mods (MultiClick n') btn))
	         in putSP tev' $ doubleClickSP n' tev'
	  where
	    (lastpath,XEvt (ButtonEvent lasttime _ _ lastmods _ lastbtn)) = last
	_ -> putSP tev $ doubleClickSP n last
