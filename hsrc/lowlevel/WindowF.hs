module WindowF(adjustBorderWidth,border_width,getBWidth,kernelF, toKernel, kernelTag,autumnize, windowKF) where
import Command
import CompOps((>+<))
import CompSP(prepostMapSP)
--import CompSP(serCompSP)
import Utils(pair,mapList)
import Direction
import Event
--import Font(FontStruct)
import Fudget
import FRequest
import Geometry(Point(..), Rect(..), origin, padd, pmax, rR)
import LayoutRequest
import LoopLow
--import Message(Message(..))
import NullF
import Path
--import SerCompF(idRightF)
import CompFfun(prepostMapLow)
import Spops
--import EitherUtils
import Data.Maybe(fromMaybe,isJust)
import Xtypes
import CmdLineEnv(argFlag)
--import DialogueIO hiding (IOError)

kernelF :: (K a b) -> F a b
kernelF (K proc) =
    let prep (High a) = High a
        prep (Low (_, b)) = Low b
        post (High a) = High a
        post (Low b) = Low (here, b)
    in  {-F-}ff (prepostMapSP prep post proc)

toLowHere = mapList (Low . pair here)

winF :: (Rect -> FRequest) -> [FRequest] -> Rect -> K a b -> F a b
winF winCmd startcmds rect w =
    putMessagesF (toLowHere (winCmd rect : startcmds)) (kernelF w)

newKTag = not (argFlag "oldKTag" True) -- trouble with autolayout...

kernelTag = if newKTag then here else turn L here
autumnize  = if newKTag then id else autumnize' where
  autumnize' [] = []
  autumnize' l = (reverse . tail . reverse) l


toKernel x = mapList (Low . pair kernelTag) x

getBWidth cws = case cws of
   [] -> Nothing
   CWBorderWidth bw':_ -> Just bw'
   _:cws -> getBWidth cws

adjustBorderWidth b (Point x y) = Point (x+b2) (y+b2) where b2 = 2*b

border_width = 0::Int -- Should agree with the value in the core for XCreateSimpleWindow in xdecode.c and ghc-dialogue/DoXRequest.hs

windowKF :: (Rect -> FRequest) -> Bool -> Bool -> [FRequest] -> Maybe Rect -> K a b -> F c d -> F (Either a c) (Either b d)
windowKF winCmd isShell nomap startcmds oplace k f =
 let ctrlSP (bw,nomap) =
       getSP $ \msg ->
       let same = ctrlSP (bw,nomap)
           pass = putSP msg 
	   passame = pass same
	   adjB b s = if isShell then s else adjustBorderWidth b s
       in case msg of
         Left (tag,cmd) -> case cmd of
            XReq (CreateMyWindow r) | tag /= kernelTag ->
		   putSP (Left (kernelTag, 
	                    XReq (CreateSimpleWindow tag r))) same
	    XCmd (ReparentToMe r w) | r == here && tag /= kernelTag ->
		   putSP (Left (kernelTag,XCmd (ReparentToMe tag w))) same
            XCmd (ConfigureWindow cws) | tag == kernelTag -> 
	        -- hopefully, this occurs before LayoutMsg
		let bw' = fromMaybe bw (getBWidth cws) in
		if bw'==bw' then pass $ ctrlSP (bw',nomap) else error "windowKF"
            LCmd (LayoutRequest req) ->
	        putSP (Left (tag,layoutRequestCmd (mapLayoutSize (adjB bw) req))) $
		same
	    _ -> passame
	 Right (tag,evt) -> case evt of
	    XResp (WindowCreated _) | tag==kernelTag -> same 
	    -- does not correspond to internal request
            LEvt (LayoutPlace (Rect p s)) -> let ads = adjB (-bw) s in
	       putsSP ([Right (tag, LEvt (LayoutPlace (Rect origin ads))),
		      Right (kernelTag, LEvt (LayoutSize ads)),
		      Right (kernelTag, LEvt (LayoutPos p))] ++   -- TH 990321
		       (if isShell then []
		        else mapList (Left. pair kernelTag)
			         ([XCmd $ moveResizeWindow (checkSize (Rect p ads))] ++
			          (if nomap || static then []
				   else [XCmd MapRaised])))) $
               ctrlSP (bw,True)
	    -- The LayoutSize message is not sent by the ordinary layout
	    -- system, but by popupGroupF for convenience, to resize the
	    -- window without moving it.
            LEvt (LayoutSize s) -> let ads = adjB (-bw) s in
	       putsSP ([Right (tag, LEvt (LayoutPlace (Rect origin ads))),
		      Right (kernelTag, LEvt (LayoutSize ads))] ++
		       (if isShell then []
		        else [(Left. pair kernelTag)
			        (XCmd $ resizeWindow (pmax ads minSize))])) $
               same
	    _ -> passame

     minSize = Point 1 1
     checkSize (Rect p s) = Rect p (pmax s minSize)
     static = isJust oplace
     startplace =
	 case oplace of
	   Nothing -> rR 0 0 10 10
	   Just p -> p
     statlimits (Rect p s) = layoutRequestCmd (plainLayout (padd p s) True True)
     wf =
	 winF winCmd
	      (startcmds ++
	       (case oplace of
		  Nothing -> []
		  Just r -> [statlimits r] ++
			    (if nomap then [] else [XCmd MapRaised])))
	      startplace
	      k
     wff = adjTag (wf>+<f)
     adjTag = if newKTag then prepostMapLow addktag removektag else id where
	    addktag ([],m) = ([L],m)
	    addktag tm = tm
	    removektag ([L],m) = ([],m)
	    removektag tm = tm

     windowf = loopThroughLowF (ctrlSP (border_width,nomap)) wff
     --windowf = windowf' ctrlSP nomap wf f
 in  case oplace of
          Nothing -> windowf
          Just place -> let prep' (High ltag) = [(ltag, LEvt (LayoutPlace place))]
                            prep' (Low (_, LEvt (LayoutPlace _))) = []
                            prep' (Low msg) = [msg]
                            post' (ltag, LCmd _) = [High ltag]
                            post' cmd = [Low cmd]
                        in  loopLow (concmapSP post') (concmapSP prep') windowf

{-
windowf' ctrlSP nomap wf f =
  let wff = wf>+<f
      windowf = loopThroughLowF (ctrlSP (border_width,nomap)) wff
  in windowf
--}
