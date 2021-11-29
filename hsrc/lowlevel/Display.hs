module Display(openDisplay) where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
import Xrequest
--import Xtypes

-- DECONSTR :: (a<->b) -> b -> (Maybe a) 
openDisplay name =
    xrequestF (OpenDisplay name)
              (let e (DisplayOpened a) = Just a
                   e _ = Nothing
               in  e)

