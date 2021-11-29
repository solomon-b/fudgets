module ToggleGroupF(toggleGroupF) where
import ButtonGroupF
import CompOps((>=^^<))
import Spops(mapstateSP)
import Fudget
--import Geometry(Line(..), Point(..), Rect(..), Size(..))
--import Message(Message)
import SerCompF(idLeftF)
import Xtypes(KeySym(..), ModState(..))

toggleGroupF :: [(ModState, KeySym)] -> (F (Either (Either Bool Bool) a) b) -> F (Either Bool a) (Either Bool b)
toggleGroupF keys f =
    let toPressed = Right . Left . Left
        toStateF = Right . Left . Right
        toFudget = Right . Right
        through = Left
        change s = (s, [toStateF s, through s])
        prep s (Right (Left ns)) = change ns
        prep s (Left BMClick) = change (not s)
        prep s (Left BMNormal) = (s, [toPressed False])
        prep s (Left BMInverted) = (s, [toPressed True])
        prep s (Right (Right m)) = (s, [toFudget m])
    in  buttonGroupF keys (idLeftF f >=^^< mapstateSP prep False)


