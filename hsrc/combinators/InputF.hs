module InputF(module InputF) where
import Fudget
import InputMsg
import CompOps
import ListF(listF)
import Placer(placerF)
import LayoutF(orientP)
import EitherUtils(stripEither)
import Spops
import InputSP
import AuxTypes() -- synonym KeySym, for hbc
import SpEither(splitSP)
import SerCompF(mapF,toBothF)

type InF a b = F a (InputMsg b)

inputPairLF orient f1 f2 = placerF (orientP orient) $ inputPairF f1 f2
inputListLF placer tfs = placerF placer $ inputListF tfs

inputPairF :: InF a1 b1 -> InF a2 b2 -> InF (a1,a2) (b1,b2)
inputPairF f1 f2 = inputPairSP >^^=< (f1>+<f2) >=^^< splitSP

inputListF :: (Eq a) => [(a, InF b c)] -> InF [(a, b)] [(a, c)] 
inputListF tfs =
  inputListSP (map fst tfs) >^^=< listF [(t,f)|(t,f)<-tfs] >=^^< concatSP

inputThroughF :: InF a a -> InF a a
inputThroughF f = stripEither>^=<(f>+<mapF InputChange)>==<toBothF

