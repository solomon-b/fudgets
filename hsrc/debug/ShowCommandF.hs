{-# LANGUAGE CPP #-}
module ShowCommandF(showCommandF) where
import CompOps((>.=<), (>=.<))
import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
--import Message(Message)
import Path(showPath)
import Debug.Trace(trace)
import Xtypes
import Command
import Event
--import ResourceIds
import Sockets
import CmdLineEnv(argFlag)
--import DialogueIO hiding (IOError)

showCommandF :: String -> (F a b) -> F a b
showCommandF s f = const (if not (argFlag s False) then f else
    let showit show' d (t, c) =
            trace (s ++ " " ++ d ++ ": " ++ showPath t ++ ": " ++ show' c
#ifdef __NHC__
		   ++ "\n"
#endif
		   )
                  (t, c)
    in  (showit show "out" >.=< f) >=.< showit show "in") x

{-
showEv e = case e of
       IOResponse (XResponse x) -> "IOResponse (XResponse ..."++show x++")"
       _ -> show e
-}

-- Hack: The following is to avoid show methods from hlib

x = [show (m :: Display),
     show (m :: Command),
     show (m :: Event),
     show (m :: WindowId),
     show (m :: Descriptor)]

m = error "module ShowCommandF"
