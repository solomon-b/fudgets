module Tables(PathTable(..), WidTable(..), updatePath, lookupPath, wid2path0,
              pruneWid, updateWid, subWids, lookupWid, path2wid0, PathTree,
	      moveWids,movePaths, prunePath) where
import Direction
--import Fudget
import Path
import Table
import PathTree
import Utils(oo)
import Xtypes

-- Most functions here should be imported from PathTree instead !!!

type WidTable = PathTree WindowId

path2wid0 = Tip

lookupWid = subTree (\(Node w _ _) -> w) noWindow

moveWids path2wid opath npath = insertTree st pt npath where
    st = subTree id Tip path2wid opath 
    pt = pruneWid path2wid opath

subWids = oo (filter (/= noWindow)) (subTree (listWids []) [])

listWids = listNodes

updateWid t path' wid = insertTree (Node wid Tip Tip) t path'

pruneWid t path' = insertTree Tip t path'

insertTree = updTree . const
updTree f t path' =
    case path' of
      [] -> f t
      L : path'' -> updLeft f t path''
      R : path'' -> updRight f t path''
      Dno n : path'' -> updateDyn f t (pos n) path''

updLeft f t path' =
    case t of
      Tip -> Node nowid (updTree f Tip path') Tip
      Node w l r -> Node w (updTree f l path') r
      Dynamic _ -> error "tables.m: updLeft (Dynamic _)"

updRight f t path' =
    case t of
      Tip -> Node nowid Tip (updTree f Tip path')
      Node w l r -> Node w l (updTree f r path')
      Dynamic _ -> error "tables.m: updRight (Dynamic _)"

updateDyn f t n path' =
    case t of
      Tip -> Dynamic (updateDyn' f DynTip n path')
      Dynamic t' -> Dynamic (updateDyn' f t' n path')

updateDyn' f DynTip 0 path' =
    DynNode (updTree f Tip path') DynTip DynTip
updateDyn' f (DynNode t l r) 0 path' = DynNode (updTree f t path') l r
updateDyn' f t n path' =
    (if n `rem` 2 == 0 then updDynLeft else updDynRight) f
                                                         t
                                                         (n `quot` 2)
                                                         path'

updDynLeft f t n path' =
    case t of
      DynTip -> DynNode Tip (updateDyn' f DynTip n path') DynTip
      DynNode t' l r -> DynNode t' (updateDyn' f l n path') r

updDynRight f t n path' =
    case t of
      DynTip -> DynNode Tip DynTip (updateDyn' f DynTip n path')
      DynNode t' l r -> DynNode t' l (updateDyn' f r n path')

nowid = noWindow

-------
type PathTable = Table (WindowId, Path)

nopath = here -- error "window not associated with a path"

wid2path0 = emptyTable

-- This part should be replaced with something more efficient!!
lookupPath wid2path wid =
    tableLookup nopath snd (wid, nopath) wid2path

-- normal code
updatePath wid2path wid path' = tableUpdate (wid, path') wid2path

movePaths wid2path opath npath = mapTable move wid2path where
  move (wid,path) = (wid,repath opath path) where
     repath [] rest = absPath npath rest
     repath (x:xs) (y:ys) | x == y = repath xs ys
     repath _ _ = path

-- should be implemented in Tree234
prunePath wid2path w = table $ filter ((/=w).fst) $ listTable wid2path
