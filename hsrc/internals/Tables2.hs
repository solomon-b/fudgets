module Tables2(DTable(..), dtable0, lookupDe, listDe, updateDe) where
import Utils(pair)
import Direction() -- instances
import Path(here,Path(..))
import Sockets(Descriptor)
import Table

type DTable = Table (Descriptor, Path)

updateDe :: Path -> [Descriptor] -> DTable -> DTable
updateDe path' ds dtable =
    table (map (`pair` path') ds ++
           filter ((/= path') . snd) (listTable dtable))

listDe :: DTable -> [Descriptor]
listDe dtable = map fst (listTable dtable)

lookupDe :: DTable -> Descriptor -> Path
lookupDe dtable de =
    tableLookup (error ("Descriptor without path: "++show de)) snd (de, here) dtable

dtable0 :: DTable
dtable0 = table []

