module TransCoord where
import Command
import Event
import Fudget
import FRequest
import Geometry(Point)
import Xrequest
import Cont(cmdContK)
import Xtypes

getWindowRootPoint :: Cont (K a b) Point
getWindowRootPoint =
    let cmd = TranslateCoordinates
        expected (CoordinatesTranslated p) = Just p
        expected _ = Nothing
    in  xrequestK cmd expected


getWindowId :: Cont (K a b) Window
getWindowId = cmdContK (XCmd GetWindowId)
	      (\r->case r of XEvt (YourWindowId w) -> Just w; _ -> Nothing)

-- Why is GetWindowId an XCommand?
