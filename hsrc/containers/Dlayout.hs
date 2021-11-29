module Dlayout(invisibleGroupF,
               simpleGroupF, unmappedGroupF, groupF, groupF',
	       sgroupF, swindowF,
               windowF,sF) where
--import Alignment(Alignment(..))
import Command
import CompFfun(prepostMapHigh)
--import CompOps((>=^^<), (>^=<),(>..=<))
import Defaults(bgColor)
import CmdLineEnv(resourceName)
import Event
import Fudget
import FRequest
import Geometry(Point(..), Rect(..), origin, pmax)
import GreyBgF(changeBg)
import LayoutRequest
import LoopLow
--import Message(Message(..))
import NullF
import Spops
--import SpEither(mapFilterSP)
import AutoLayout(autoLayoutF',nowait)
import Sizing(Sizing(..))
import EitherUtils(stripEither)
--import Utils(oo)
import WindowF
import Xtypes
--import Placer(placerF,spacerF)
--import Spacers
--import AutoPlacer(autoP)
import ParSP
--import CompSP
import Path(turn,here)
import Direction(Direction(..))

addEventMask addmask =
    let addem [] = []
        addem (CWEventMask mask : wattrs) =
            CWEventMask (addmask ++ mask) : wattrs
        addem (wattr : wattrs) = wattr : addem wattrs
    in  addem

shell :: Bool -> (F a b) -> F a b
shell nomap f =
    let eventmask = [StructureNotifyMask,KeyPressMask,KeyReleaseMask,FocusChangeMask]

        prep ss@(osize,sizeq, Just ltag) (Low (tag, XEvt (ConfigureNotify (Rect _ nsize) _)))
           | tag == kernelTag = case sizeq of 
	     (size:sizeq') | size == nsize -> ((nsize,sizeq',Just ltag),[])
	     _ -> if nsize == osize then (ss,[]) else
	        ((nsize,sizeq,Just ltag),[(ltag,LEvt $ LayoutPlace (Rect origin nsize))])
        prep s (Low (t,e@(XEvt (FocusIn  {})))) | t == kernelTag = (s,[(focusMgrTag, e)])
        prep s (Low (t,e@(XEvt (FocusOut {})))) | t == kernelTag = (s,[(focusMgrTag, e)])
        prep s (Low (t,e@(XEvt (KeyEvent {})))) | t == kernelTag = (s,[(focusMgrTag, e)])
        prep s (Low msg) = (s, [msg])
        prep (osize,sizeq, _) (High (tag, nsize)) = 
	  ((osize,sizeq++ (if null sizeq || last sizeq /= nsize then [nsize] else []), Just tag),
	   [(tag, LEvt $ LayoutPlace (Rect origin nsize))])

        focusMgrTag = turn R $ turn L here -- hardwired assumption
        minSize = Point 1 1

        post nomap' (tag, LCmd lreq) = case lreq of
	  LayoutRequest (Layout {minsize=size}) ->
            (True,
             High (tag, size) :
             toKernel ([XCmd $ resizeWindow (pmax minSize size)] ++
                       (if nomap' then [] else [XCmd $ MapRaised])))
            -- we should actually wait with MapRaised until f reports OK somehow...
          _ -> (nomap',[]) {- filter all other layout msgs -}
        post nomap' (tag, XCmd (ChangeWindowAttributes wattrs)) | tag == kernelTag =
            (nomap',
             [Low (tag, XCmd $ ChangeWindowAttributes (addEventMask eventmask wattrs))])
        post nomap' (_,XCmd MeButtonMachine) = (nomap', [])
        post nomap' cmd@(tag, XCmd (ChangeWindowAttributes wattrs)) = (nomap',[Low cmd])
        post nomap' cmd = (nomap', [Low cmd])
        startcmds = toKernel [XCmd $ ChangeWindowAttributes [CWEventMask []],
                              XCmd $ SetWMHints True]
    in  loopLow (mapstateSP' post nomap)
                (mapstateSP prep ((Point 10 10),[], Nothing)) {- 10 10 from windowKF... -}
                (myAppendStartF startcmds f)

mapstateSP' f s0 =
    getSP (\x ->
           case f s0 x of
             (s, y) -> putsSP y (mapstateSP' f s))
{-
-- causes a space leak with nhc13 and ghc-7.6
mapstateSP' f s0 =
    getSP (\x ->
           let (s, y) = f s0 x
           in putsSP y (mapstateSP' f s))
-}
-- myAppendStart will let f speak all its initial msgs, not just the first one.
myAppendStartF cmds (F f) = {-F-}ff $ parSP f (putsSP cmds nullSP)

windowF :: [FRequest] -> (K a b) -> F a b
windowF cmds = swindowF cmds Nothing

swindowF cmd oplace k = 
    prepostMapHigh Left stripEither (group0F sizing False cmd oplace k Nothing)
  where sizing = if oplace==Nothing then Static else Dynamic

sF nomap pos lc k f =
    let r = Nothing -- temporary
        p =
            case r of
              Just (Rect p _) -> Just p
              Nothing -> pos
        lc' =
            case p of
              Just p' -> XCmd (SetNormalHints p') : XCmd (moveWindow p') : lc
              Nothing -> lc
    in shell nomap
          (windowKF (XReq . flip CreateRootWindow resourceName) True nomap lc' r (bgK k) f)

group0F sizing nomap cmds r k mf =
    case mf of
      Nothing -> w nullF
      Just f -> w $ autoLayoutF' nowait sizing f
  where w = windowKF (XReq . CreateMyWindow) False nomap cmds r k 

sgroupF sizing cmds r k = group0F sizing False cmds r k . Just
groupF' sizing cmds = sgroupF sizing cmds Nothing
groupF = groupF' Dynamic

unmappedGroupF sizing cmds k = group0F sizing True cmds Nothing k . Just


simple sf sizing startcmds k w =
    prepostMapHigh Right stripEither (sf sizing startcmds k w)

bgK = changeBg bgColor

--sGF :: (K a b) -> [WindowAttributes] -> (F a b) -> F a b
sGF sizing k wattrs =
  simple groupF' sizing [XCmd $ ChangeWindowAttributes wattrs] k

simpleGroupF = sGF Dynamic (bgK nullK)
invisibleGroupF sizing cmds = 
     sGF sizing (bgK (putsK (map Low cmds) nullK))

