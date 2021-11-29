module QueryTree where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
--import Path(Path(..))
import Xrequest
--import Xtypes

queryTreeK = 
    xrequestK QueryTree gotit
  where
    gotit (TreeQueried r p cs) = Just (r,p,cs)
    gotit _ = Nothing

queryTreeF = 
    xrequestF QueryTree gotit
  where
    gotit (TreeQueried r p cs) = Just (r,p,cs)
    gotit _ = Nothing

{-
(defaultRootWindowK,defaultRootWindowF) =
    (xrequestK DefaultRootWindow gotit,xrequestF DefaultRootWindow gotit)
  where
    gotit (GotDefaultRootWindow w) = Just w
    gotit _ = Nothing

Doesn't work???
-}

defaultRootWindowK = 
    xrequestK DefaultRootWindow gotit
  where
    gotit (GotDefaultRootWindow w) = Just w
    gotit _ = Nothing

defaultRootWindowF = 
    xrequestF DefaultRootWindow gotit
  where
    gotit (GotDefaultRootWindow w) = Just w
    gotit _ = Nothing

