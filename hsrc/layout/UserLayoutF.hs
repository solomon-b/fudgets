module UserLayoutF where
import Fudget
import FRequest
--import Command
--import Event
import Geometry(Rect)
--import CompOps
import Spops(concatMapSP)
import CompSP(preMapSP,serCompSP)
import LayoutRequest
--import Message(Message(..))

userLayoutF :: F a b -> F (Either (Path,Rect) a) (Either (Path,LayoutMessage) b)
userLayoutF (F fud) = F{-ff-} (concatMapSP post `serCompSP` fud `preMapSP` pre)
  where
    pre msg =
      case msg of
        High (Right x) -> High x
	High (Left (p,place)) -> Low (p,LEvt (LayoutPlace place))
	Low pev -> Low pev
    post msg =
      case msg of
        High x -> [High (Right x)]
	Low (p,LCmd req) -> [High (Left (p,req))]
	--Low d@(p,XCmd DestroyWindow) -> [Low d] --,High (Left (p,LayoutDestroy))]
	Low pcmd -> [Low pcmd]
