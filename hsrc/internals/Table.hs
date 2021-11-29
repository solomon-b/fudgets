module Table(table, mapTable, listTable, tableUpdate, tableLookup, emptyTable,
             Table) where
import Tree234

newtype Table a = T (Tree234 a) deriving (Eq, Ord,Show)

emptyTable = T initTree234

tableLookup n j x (T t) = treeSearch n j (keyCmp x) t

tableUpdate x (T t) = T (update' x t)

update' x = treeAdd const keyCmp x

mapTable f (T t) = T (treeMap f t)

listTable (T t) = treeList t

table xs = T (treeFromList const keyCmp xs)

keyCmp (a, _) (b, _) lt eq gt =
    if a == b then eq else if a < b then lt else gt

