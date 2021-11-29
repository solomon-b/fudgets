module DynListLF(dynListLF) where
import Fudget
import CompOps((>+<))
import DynListF
import Dynforkmerge(DynMsg(..))
import Loops(loopThroughRightF)
import SerCompF(concatMapF)
import UserLayoutF
--import Geometry
import LayoutRequest
import LayoutSP(dynLayoutMgrF)
import LayoutF(LayoutDirection(..))

dynListLF :: Placer -> F (Int, DynFMsg a b) (Int, b)
dynListLF lter =
  loopThroughRightF
    (concatMapF ctrl) (dynLayoutMgrF 0 Forward lter>+<userLayoutF dynListF)

ctrl msg =
  case msg of
    Right dyn@(i,dynmsg) ->
      case dynmsg of
        DynCreate _ -> [toMgrDyn (i,True),toDyn dyn]
	DynDestroy -> [toMgrDyn (i,False),toDyn dyn]
	DynMsg _ -> [toDyn dyn]
    Left (Left pp) -> [toDynL pp]
    Left (Right (Left ll)) -> [toMgr ll]
    Left (Right (Right x)) -> [out x]

toDyn=Left. Right. Right
toDynL=Left. Right. Left
toMgr=Left. Left. Left
toMgrDyn=Left. Left. Right
out=Right
