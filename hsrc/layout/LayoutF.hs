module LayoutF(serCompLF, compLF, listLF, untaggedListLF,
	       --rbLayoutF,
               nullLF, holeF, holeF', lF, LayoutDirection(..),orientP) where
--import TableP
import CompF
import CompOps((>^=<))
import Fudget
import FRequest
import Geometry() -- instances
import LayoutDir(Orientation(..))
import LayoutRequest
import Placers
import ListF
import NullF
import FudgetIO
import SerCompF(serCompF)
import Utils(number)
--import Xtypes
import AlignP(revP)
import Placer(placerF)

data LayoutDirection = Forward | Backward  deriving (Eq, Ord,Show)

holeF' s = putLow (layoutRequestCmd (plainLayout s False False)) nullF
holeF = holeF' 0
nullLF = holeF

--listLF :: Eq a => Placer -> [(a, F b c)] -> F (a, b) (a, c)
listLF placer fl = lF (length fl) Forward placer (listF fl)

untaggedListLF :: Placer -> [F a b] -> F (Int, a) b
untaggedListLF layout fs = snd >^=< listLF layout (number 0 fs)

compLF = cLF compF
serCompLF = cLF serCompF
--rbLayoutF sep = lF 3 Forward (rightBelowP sep)

cLF :: ((F a b) -> (F c d) -> F e f) -> (F a b,Orientation) -> F c d -> F e f
cLF cF (f1,ori) f2 = lF 2 Forward (orientP ori) (cF f1 f2)

lF :: Int -> LayoutDirection -> Placer -> (F a b) -> F a b
lF 0 _ _ f = nullLF
lF nofudgets dir placer f = placerF placer' f where 
     placer' = if dir == Backward then revP placer else placer

orientP :: Orientation -> Placer
orientP ori =
   case ori of
     Above -> verticalP
     Below -> revP verticalP
     LeftOf -> horizontalP
     RightOf -> revP horizontalP
