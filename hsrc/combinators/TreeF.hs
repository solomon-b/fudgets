module TreeF where
import BranchF
import CompFfun(prepostMapHigh')
import Utils(pair)
import Fudget

data Tree a = Leaf a | Branch (Tree a) (Tree a)  deriving (Eq, Ord)

treeF :: Tree (a, F b c) -> F (Path, b) (a, c)
treeF = F{-ff-} . treeF'

treeF' :: Tree (a, F b c) -> FSP (Path, b) (a, c)
treeF' (Leaf (t, f)) = leafF t f
treeF' (Branch l r) = branchFSP (treeF' l) (treeF' r)

leafF t (F sp) =
    let pre ([], x) = x
        pre _ = error "unknown path in treeF"
    in  prepostMapHigh' pre (pair t) sp
