module ListF(listF,untaggedListF) where
import CompF
import CompSP(prepostMapSP)
import CompSP(preMapSP)
import CompOps((>^=<),(>=^^<))
import Direction
import Fudget
--import ListMap(lookupWithDefault)
--import Message(Message(..))
--import NullF
--import Path(Path(..))
import Spops
import TreeF
import Utils(number,pair)
import HbcUtils(apSnd,lookupWithDefault)
import LayoutHints

untaggedListF :: [F a b] -> F a b
untaggedListF fs = snd >^=< listF tfs >=^^< concatMapSP broadcast
  where
    tfs = number 0 fs
    ns = map fst tfs
    broadcast x = map (`pair` x) ns

listF :: {-Prelude.-}Eq a => [(a, F b c)] -> F (a, b) (a, c)
listF = layoutHintF listHint . F{-ff-} . listF'

listF' :: Eq a => [(a, F b c)] -> FSP (a, b) (a, c)
listF' [(tag, F w)] =
    let prepinp (High (t, a)) =
            if t == tag then High a else error "Unknown tag in listF"
        prepinp (Low tev) = Low tev
        prepout (High b) = High (tag, b)
        prepout (Low cmd) = Low cmd
    in  prepostMapSP prepinp prepout w
listF' [(ltag, lw), (rtag, rw)] =
    let prepinp (High (tag, a)) =
            if tag == ltag then
                High (Left a)
            else
                if tag == rtag then
                    High (Right a)
                else
                    error "Unknown tag in listF"
        prepinp (Low tev) = Low tev
        prepout (High (Left b)) = High (ltag, b)
        prepout (High (Right b)) = High (rtag, b)
        prepout (Low cmd) = Low cmd
        F lwrw = compF lw rw
    in  prepostMapSP prepinp prepout lwrw
listF' [] = nullSP
listF' wtab =
    let tree = balancedTree wtab
        paths = pathtab tree
        prepinp (High (tag, a)) =
            let path' = lookupWithDefault paths (error "Unknown tag in listF") tag
            in  High (path', a)
        prepinp (Low tev) = Low tev
    in  preMapSP (treeF' tree) prepinp

pathtab (Leaf (t, _)) = [(t, [])]
pathtab (Branch l r) =
    map (apSnd (L :)) (pathtab l) ++ map (apSnd (R :)) (pathtab r)

balancedTree xs =
    case xs of
      [x] -> Leaf x
      _ -> let (l, r) = split2 xs
           in  Branch (balancedTree l) (balancedTree r)

split2 l =
    let sp = length l `quot` 2
    in  (take sp l, drop sp l)

