module PathTree
  (
    PathTree(..),DynTree(..),
    -- PathTree should be abstract...
    emptyPathTree,updateNode,mapPathTree,node,subTree,listNodes,
    attrMapPathTree,
    pruneTree,
    pos,unpos,spineVals
  ) where
import Direction
--import Path
import HbcUtils(apSnd)
import Maptrace(ctrace)

data PathTree n
   = Node n (PathTree n) (PathTree n)
   | Dynamic (DynTree (PathTree n))
   | Tip
   deriving (Eq, Ord, Show)

data DynTree n
  = DynNode n (DynTree n) (DynTree n)
  | DynTip 
  deriving (Eq, Ord, Show)

emptyPathTree = Tip
lookupPath f z = subTree (\(Node w _ _) -> f w) z
subNodes x = subTree (listNodes []) [] x

listNodes ns t =
    case t of
      Tip -> ns
      Node n l r -> listNodes (listNodes (n : ns) l) r
      Dynamic dt -> listDynNodes ns dt

listDynNodes ns dt =
    case dt of
      DynTip -> ns
      DynNode t l r -> listNodes (listDynNodes (listDynNodes ns r) l) t

subTree f z Tip _ = z
subTree f z (Dynamic dt) p = dynSelect (subTree f z) z dt p
subTree f z n [] = f n
subTree f z (Node _ l _) (L : path') = subTree f z l path'
subTree f z (Node _ _ r) (R : path') = subTree f z r path'
--subTree _ _ t p = error ("subTree _ _ "++show t++" "++show p)
-- Other cases of ill matching trees/paths return z, so why shouldn't this one?
subTree _ z t p = ctrace "subTree" ("subTree _ _ "++show t++" "++show p) z

{-
dynSubTree f z DynTip _ _ = z
dynSubTree f z (DynNode t _ _) 0 path' = subTree f z t path'
dynSubTree f z (DynNode _ l r) n path' =
    dynSubTree f z (if n `rem` 2 == 0 then l else r) (n `quot` 2) path'
-}
dynSelect f z dn (Dno n:p) = dynSelect' dn (pos n) where
   dynSelect' DynTip _ = z
   dynSelect' (DynNode t l r) n = if n == 0 then f t p else
	     dynSelect' (if n `rem` 2 == 0 then l else r) (n `quot` 2)
dynSelect f z dn _ = z

pruneNode e = insertTree e Tip
insertTree n = updTree id n . const
pruneTree i e t path = updTree i e (const Tip) t path

updateNode i e t path f = updTree i e g t path
  where g Tip = Node (f e) Tip Tip
        g (Node n l r) = Node (f n) l r

updTree i e f t path' =
    case path' of
      [] -> f t
      L : path'' -> updLeft i e f t path''
      R : path'' -> updRight i e f t path''
      Dno n : path'' -> updateDyn i e f t (pos n) path''

updLeft i e f t path' =
    case t of
      Tip -> Node e (updTree i e f Tip path') Tip
      Node n l r -> Node (i n) (updTree i e f l path') r
                    -- !!! space leak danger if i n is never used
		    -- need stingy evaluation!!
      Dynamic _ -> error "PathTree.hs: updLeft (Dynamic _)"

updRight i e f t path' =
    case t of
      Tip -> Node e Tip (updTree i e f Tip path')
      Node n l r -> Node (i n) l (updTree i e f r path')
                    -- !!! space leak danger if i n is never used
		    -- need stingy evaluation!!
      Dynamic _ -> error "PathTree.hs: updRight (Dynamic _)"

updateDyn i e f t n path' =
    case t of
      Tip -> Dynamic (updateDyn' i e f DynTip n path')
      Node _ _ _ -> Dynamic (updateDyn' i e f DynTip n path') -- throwing away part of the tree !!!
      Dynamic t' -> Dynamic (updateDyn' i e f t' n path')

updateDyn' i e f DynTip 0 path' =
    DynNode (updTree i e f Tip path') DynTip DynTip
updateDyn' i e f (DynNode t l r) 0 path' = DynNode (updTree i e f t path') l r
updateDyn' i e f t n path' =
    (if n `rem` 2 == 0 then updDynLeft else updDynRight) i e f
                                                         t
                                                         (n `quot` 2)
                                                         path'

updDynLeft i e f t n path' =
    case t of
      DynTip -> DynNode Tip (updateDyn' i e f DynTip n path') DynTip
      DynNode t' l r -> DynNode t' (updateDyn' i e f l n path') r

updDynRight i e f t n path' =
    case t of
      DynTip -> DynNode Tip DynTip (updateDyn' i e f DynTip n path')
      DynNode t' l r -> DynNode t' l (updateDyn' i e f r n path')

pos :: Int->Int
pos 0 = 0
pos n = if n < 0 then (-2) * n else 2 * n + 1

unpos :: Int->Int
unpos n =
  if even n
  then -(n `quot` 2)
  else n `quot` 2

mapPathTree f t =
  case t of
    Node n lt rt -> Node (f n) (mapPathTree f lt) (mapPathTree f rt)
    Dynamic dt -> Dynamic (mapDyn (mapPathTree f) dt)
    Tip -> Tip

mapDyn f dt =
  case dt of
    DynNode n lt rt -> DynNode (f n) (mapDyn f lt) (mapDyn f rt)
    DynTip -> DynTip

attrMapPathTree :: (i -> [s] -> a -> (i,s,b)) -> i -> PathTree a -> 
		   ([s],PathTree b)
attrMapPathTree f i t = case t of
   Node n lt rt -> ([s],Node n' lt' rt') where
      (sl,lt') = attrMapPathTree f i' lt
      (sr,rt') = attrMapPathTree f i' rt
      (i',s,n') = f i (sl++sr) n
      -- should perhaps extract ++ and []
   Tip -> ([],Tip)
   Dynamic dt -> apSnd Dynamic (attrMapDyn f i dt)

attrMapDyn f i dt = case dt of
   DynTip -> ([],DynTip)
   DynNode t lt rt -> (s++sl++sr,DynNode t' lt' rt') where
	     --        order?
      (sl,lt') = attrMapDyn f i lt
      (sr,rt') = attrMapDyn f i rt
      (s,t') = attrMapPathTree f i t

spineVals t p = case t of
	  Tip -> []
	  Node v l r -> v : case p of
	       L:p' -> spineVals l p'
	       R:p' -> spineVals r p'
	       _    -> []
          Dynamic dt -> dynSelect spineVals [] dt p

node t =
  case t of
    Node i lt rt -> (Just i,children lt (children rt []))
    Tip -> (Nothing,[])
    Dynamic dt -> (Nothing,dynChildren dt [])

children t ts =
  case t of
    Tip -> ts
    Node _ _ _ -> t:ts
    Dynamic dt -> dynChildren dt ts

dynChildren dt ts =
  case dt of
    DynTip -> ts
    DynNode t lt rt -> (children t . dynChildren lt . dynChildren rt) ts
      -- !! order??
