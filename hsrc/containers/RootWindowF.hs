module RootWindowF where
import Command
--import Event
--import Font(FontStruct)
--import Fudget
--import Message(Message(..))
import Geometry(Rect(..),origin,pP)
--import Path
import WindowF(kernelF)
--import NullF(putK)
--import Xrequest(xrequestF)
import Xcommand(xcommandK)
import QueryTree
import UserLayoutF(userLayoutF)
import CompOps
import SerCompF(absF)
import Loops(loopThroughRightF)
import Spops(mapSP)
import AutoLayout(autoLayoutF)
--import CmdLineEnv(argReadKey)
--import NullF(nullF)
import WindowF(kernelTag)

{-
rootWindowF k =
    defaultRootWindowF $ \ root ->
    kernelF (putK (Low (ReparentToMe kernelTag root)) k) >*< nullF
-}
  
rootWindowF k =
    defaultRootWindowF $ \ root ->
    kernelF (xcommandK (SelectWindow root) k)
  
rootGroupF k f =
    k' >+< f'
  where
    k' = kernelF (
            defaultRootWindowK $ \ root ->
            xcommandK (ReparentToMe kernelTag root) k)
    f' = loopThroughRightF (absF ctrlSP) (userLayoutF (autoLayoutF f))
    ctrlSP = mapSP ctrl
    ctrl msg =
      case msg of
        Left (Left (path,req)) -> Left (Left (path,screenrect))
	Left (Right x) -> Right x
	Right x -> Left (Right x)
	
    screenrect = Rect origin (pP 1152 900)
    -- !!! no Read instance...
    --screenrect = Rect origin (argReadKey "screensize" (pP 1152 900)) -- !!!
