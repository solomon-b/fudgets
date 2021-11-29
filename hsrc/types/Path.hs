module Path(showPath, subPath, absPath, path, turn, here, Path(..)) where
import Direction
import Utils(lhead)

type Path = [Direction]

here :: Path
here = []

turn :: Direction -> Path -> Path
turn dir p = dir : p

path :: Path -> (Direction, Path)
path (dir : p) = (dir, p)
path [] = error "path.m: path []"

absPath :: Path -> Path -> Path
absPath absp relp = absp ++ relp

subPath :: Path -> Path -> Bool
subPath subp p = length subp <= length p && lhead subp p == subp

showPath :: Path -> String
showPath =
    concatMap (\x ->
               case x of
                 L -> "L"
                 R -> "R"
                 Dno n -> "N(" ++ show n ++ ")")

