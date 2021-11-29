module Dynforkmerge(DynMsg(..), DynSPMsg(..), dynforkmerge) where
import SP
import Utils(part)

data DynMsg a b = DynCreate b |
                  DynDestroy |
                  DynMsg a 
                  deriving (Eq, Ord)

type DynSPMsg a b = DynMsg a (SP a b)

dynforkmerge :: Eq a => SP (a, DynSPMsg b c) (a, c)
dynforkmerge = dfm []

dfm dynxsps =
    GetSP (\msg ->
           case msg of
             (t, DynCreate dynsp) -> dfmout t dynsp dynxsps
             (t, DynMsg msg') -> dfmin t msg' dynxsps
             (t, DynDestroy) -> dfmrm t dynxsps)

dfmout t dynsp dynxsps =
    case dynsp of
      PutSP y sp' -> PutSP (t, y) (dfmout t sp' dynxsps)
      GetSP xsp -> dfm ((t, xsp) : dynxsps)
      NullSP -> dfm dynxsps

dfmin t msg dynxsps =
    case part ((== t) . fst) dynxsps of
      ([], _) -> dfm dynxsps
      ([(_, xsp)], dynxsps') -> dfmout t (xsp msg) dynxsps'
      _ -> error "Same tag used twice in dynforkmerge (or dynListF)."

dfmrm t dynxsps = dfm (filter ((t /=) . fst) dynxsps)
