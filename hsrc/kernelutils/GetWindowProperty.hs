module GetWindowProperty(getWindowPropertyK,getGeometryK) where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
--import Path(Path(..))
import Xrequest
--import Xtypes

getWindowPropertyK offset prop delete req_type =
    let gotit (GotWindowProperty typ format nitems after str) =
            Just (typ, format, nitems, after, str)
        gotit _ = Nothing
    in  xrequestK (GetWindowProperty offset prop delete req_type) gotit

getGeometryK = xrequestK GetGeometry
   (\r -> case r of GotGeometry r bw d -> Just (r,bw,d); _ -> Nothing)
