module Editfield(EditField, moveField, replaceField, setFieldDir, getField,getField',
                 deselectField, getSelection, getLastLineNo, getAft, getBef, getLnoEdge,
                 createField, dirint, splitnl, nlines) where
import HbcUtils(breakAt)
import Edtypes

--export EditField,newline,nlines,splitnl,
data EditField = F EDirection String String String Int Int 
                 deriving (Eq, Ord)

--               bef     sel   aft     ^  lastlineno
--                            lno for selection edge

nlines s = length (filter (== newline) s)

splitnl = breakAt newline

dirint ERight = 1
dirint ELeft = -1

befaft dir bef sel aft =
    if dir == ELeft then (bef, sel ++ aft) else (sel ++ bef, aft)

createField s = F ERight [] [] s 0 (nlines s)
getLnoEdge (F dir bef sel aft elno lno) =
    (elno, befaft dir bef sel aft)
getBef (F dir bef sel aft elno lno) = bef
getAft (F dir bef sel aft elno lno) = aft
getLastLineNo (F dir bef sel aft elno lno) = lno
getSelection (F dir bef sel aft elno lno) =
    if dir == ELeft then sel else reverse sel

deselectField (F dir bef sel aft elno lno) =
    let (bef', aft') = befaft dir bef sel aft
    in  F dir bef' [] aft' elno lno

getField f = reverse (getBef f) ++ getSelection f ++ getAft f
getField' f = (reverse (getBef f),getSelection f,getAft f)

setFieldDir ndir field@(F dir bef sel aft elno lno) =
    if ndir == dir then
        field
    else
        let elno' = elno + dirint ndir * nlines sel
        in  F ndir bef (reverse sel) aft elno' lno

replaceField (F dir bef sel aft elno lno) s =
    let linessel = nlines sel
        liness = nlines s
        elno' = elno + liness - (if dir == ERight then linessel else 0)
        lno' = lno + (liness - linessel)
    in  F ERight (reverse s ++ bef) [] aft elno' lno'

moveField :: IsSelect -> EditField -> EditStopFn -> (EditField,String)
moveField issel (F dir bef sel aft elno lno) sf =
    let mo dir' bef' sel' aft' elno' acc sf =
            let stop = (F dir' bef' sel' aft' elno' lno, acc)
                (b, a) = befaft dir' bef' sel' aft'
            in case sf b a of 
		 EdStop -> stop
		 EdGo wdir sf' -> 
		    let next c dir'' bef'' sel'' aft'' =
			    let elno'' =
				    if c == newline then
					dirint wdir + elno'
				    else
					elno'
			    in  mo dir'' bef'' sel'' aft'' elno'' (c:acc) sf'
		    in  if wdir == dir' || null sel' then
			    case wdir of
			      ELeft -> case bef' of
					 [] -> stop
					 c:bef'' -> next c wdir bef'' (c:sel') aft'
			      ERight -> case aft' of
					  [] -> stop
					  c:aft'' -> next c wdir bef' (c:sel') aft''
			else
			    let c:sel'' = sel'
			    in  case dir' of
				  ELeft -> next c dir' (c:bef') sel'' aft'
				  ERight -> next c dir' bef' sel'' (c:aft')
        (field, acc) = mo dir bef sel aft elno [] sf
    in  if issel then (field, acc) else (deselectField field, [])

