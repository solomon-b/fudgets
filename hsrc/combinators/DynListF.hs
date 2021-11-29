module DynListF(dynF, DynFMsg(..), dynListF) where
import Command
import FRequest
--import Xtypes
import CompOps((>^=<),(>=^^<))
import CompSP
import Dynforkmerge
import Fudget
--import Message(Message(..))
import NullF
import Spops
import Direction
import Path
import LayoutRequest

type DynFMsg a b = DynMsg a (F a b)

dynListF :: F (Int, DynFMsg a b) (Int, b)
dynListF =
    let prep (High (t, DynMsg m)) = [High (t, DynMsg (High m))]
        prep (High (t, DynDestroy)) = 
	    map tag [XCmd DestroyWindow,LCmd LayoutDestroy] 
		++ [High (t, DynDestroy)] 
	    where tag m = Low (turn (Dno t) here, m)
        prep (High (t, DynCreate (F f))) = [High (t, DynCreate f)]
        prep (Low ([], ev)) = []
        prep (Low (tag, ev)) =
            case path tag of
              (Dno i, tag') -> [High (i, DynMsg (Low (tag', ev)))]
	      _ -> [] -- wrong address!
        post (High (t, High m)) = High (t, m)
        post (High (i, Low (tag, cmd))) = Low (turn (Dno i) tag, cmd)
        post (Low c) = Low c
    in F{-ff-} $ serCompSP (postMapSP post (idLowSP dynforkmerge)) (concmapSP prep)

dynF :: (F a b) -> F (Either (F a b) a) b
dynF f0 =
    let prep (Left f) = [(0, DynDestroy), (0, DynCreate f)]
        prep (Right m) = [(0, DynMsg m)]
    in (snd >^=< startupF [(0, DynCreate f0)] dynListF) >=^^< concmapSP prep
