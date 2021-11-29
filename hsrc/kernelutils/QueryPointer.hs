module QueryPointer(queryPointerK) where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
import Xrequest
--import Xtypes

queryPointerK =
    let f (PointerQueried s r w m) = Just (s, r, w, m)
        f _ = Nothing
    in  xrequestK QueryPointer f

