module AlignP where
--import Alignment(Alignment(..))
--import Geometry(Line(..), Point(..), Rect(..), Size(..), padd, psub, rR)
--import LayoutDir(LayoutDir)
import LayoutRequest
import Utils(mapPair, number, swap)
import HbcUtils(apSnd)
import List2(sort)
--import Spacers

-- placer operations

idP :: Placer
idP = P $ \ [req] -> (req, (: []))

--revP :: Placer -> Placer
revP = mapP revP'
  where revP' placer = apSnd (reverse .) . placer . reverse

mapP f (P p) = P (f p)

flipP :: Placer -> Placer
flipP = mapP flipP'
  where
    flipP' placer = mapPair (flipReq,flipP2) . placer . map flipReq
    flipP2 p2 = map flipRect.p2.flipRect

permuteP :: [Int] -> Placer -> Placer
permuteP perm = mapP permuteP'
  where
    permuteP' placer = apSnd (rpermf .) . placer . fpermf
    rperm = (map snd . sort . map swap . number 0) perm
    fpermf = permf perm
    rpermf = permf rperm
    permf perm xs = map (xs!!) perm
