module Layoutspec(marginHVAlignLs, hvAlignLs, marginLs, sepLs,hBoxLs', vBoxLs',
        placeLs,spaceLs,revLs, permLs, leafLs, layoutF, Layout) where
import AlignP
--import Alignment(Alignment(..))
import Fudget
--import Geometry(Point, Rect, Size(..))
--import LayoutDir(LayoutDir, Orientation)
import LayoutF(LayoutDirection(..), lF)
import LayoutRequest
import Placers
import Spacers
import Utils(oo,unconcat)

data Layout = LeafL | NodeL Placer [Layout]  --deriving (Eq, Ord)

layoutF :: Layout -> (F a b) -> F a b
layoutF layout f =
    let (n, lter) = layouter layout
    in  lF n Forward lter f

leafLs = LeafL

revLs LeafL = LeafL
revLs (NodeL lter ls') = NodeL (revP lter) ls'

npermLs _    LeafL = LeafL
permLs perm (NodeL lter ls') = NodeL (permuteP perm lter) ls'

modLs ltermod l =
    case l of
      LeafL -> NodeL (ltermod idP) [LeafL]
      NodeL lter ls' -> NodeL (ltermod lter) ls'

placeLs = NodeL
spaceLs = modLs . spacerP

vBoxLs' = placeLs . verticalP'
hBoxLs' = placeLs . horizontalP'

sepLs = spaceLs . sepS
marginLs = spaceLs . marginS
hvAlignLs = oo spaceLs hvAlignS
marginHVAlignLs sep ha va = spaceLs (marginHVAlignS sep ha va)

--layouter :: Layout -> (Int, Placer)
layouter layout =
    case layout of
      LeafL -> (1, idP)
      NodeL lter ls' -> let (ns, lters) = unzip (map layouter ls')
                        in  (sum ns, combl lter ns lters)

combl rootlter ns nodelters = P $ \ leafreqs ->
    let (nodereqs, noderess) =
            unzip (zipWith unP nodelters (unconcat ns leafreqs))
        (rootreq, rootres) = unP rootlter nodereqs
        leafres rootrect = concatMap apply (zip noderess (rootres rootrect))
    in  (rootreq, leafres)

apply (f, x) = f x

