module MineField(MInput(..),MOutput(..),Symbol(..),Coord(..),play,playSP,initMineCount,mineFieldSize) where
--import RandomHBC(randomInts)
import System.Random(StdGen,randomRs,split)
import Data.List(union, intersect)
import Fudgets(getSP,putsSP) -- SP operations
import Data.Array
import Data.List((\\),nub)

type Coord = (Int,Int)			-- mine field coordinate
type RandInt = Int
type MineCount = Int
type MineField = Array (Int,Int) Bool 	-- number of mines and the array representing the mine field
data Moves = Moves Coord [Coord] [Coord] -- current pos, places, border
	     deriving (Eq, Ord{-, Text-})

mineFieldSizeX, mineFieldSizeY :: Int
mineFieldSizeX = 12
mineFieldSizeY = 12
mineFieldSize :: Coord
mineFieldSize = (mineFieldSizeX, mineFieldSizeY)

newField :: StdGen -> MineCount -> MineField			-- random numbers gives a new field
newField g n =
  let (nx, ny) = mineFieldSize
    --rs = newRands r
    --xs = map (`rem` nx) (everyOther rs)
    --ys = map (`rem` ny) (everyOther (tail rs))
      (g1,g2) = split g
      ps = randomRs (1,nx) g1 `zip` randomRs (1,ny) g2
      xys = take ((n `min` (nx * ny)) `max` 0) $
            nub (ps \\ [(1,1),mineFieldSize])
      b = ((0,0),(nx+1,ny+1))
  in  array b [(p, p `elem` xys) | p<-range b]



--newRands r = randomInts ((r `max` 1) `min` 2147483562) (((r+123457) `max` 1) `min` 2147483398)
--everyOther (x:_:xs) = x:everyOther xs

initMoves = Moves (1,1) [(1,1)] [(1,2), (2,1), (2,2)]

moveTo :: Moves -> Coord -> Maybe Moves
moveTo (Moves _ visited border) xy | xy `elem` border =
        let visited' = xy:visited
	in  Just (Moves xy visited' (union border (neighbours xy) \\ visited'))
moveTo (Moves _ visited border) xy | xy `elem` visited =
	Just (Moves xy visited border)
moveTo _ _ = Nothing
	
neighbours :: Coord -> [Coord]
neighbours (x,y) = 
	let (xm,ym) = mineFieldSize
	in  [(xi,yi) | xi<-[x-1..x+1], xi>0 && xi<=xm, yi<-[y-1..y+1], yi>0 && yi<=ym]

data Symbol = SBlank | SNumber Int | SCurrent Int | SFree | SBomb | SBombX | SBombDone deriving (Eq, Ord, Show)
data MInput = Move Coord | Bomb Coord | Free Coord | Hint | New | SetSize Int | Noop deriving (Eq, Ord{-, Text-})
data MOutput = Repaint [(Coord, Symbol)] | Msg String | Status String deriving (Eq, Ord{-, Text-})
type Counters = (Int,Int) -- moves, hints
type Bombs = [Coord]
data State = Go StdGen MineCount MineField Moves Counters Bombs Bombs
           | Stop StdGen MineCount
           --deriving (Eq, Ord{-, Text-})

initMineCount :: MineCount
initMineCount = 26
newState :: StdGen -> MineCount -> State
newState g n = Go g1 n (newField g2 n) initMoves (0, 0) [] []
  where (g1,g2) = split g
doMove :: State -> MInput -> (State, [MOutput])
doMove s@(Go rs n mf ms c@(i,j) bs fs) (Move xy) =
    if xy `elem` bs then
	(s, [])
    else
	case moveTo ms xy of
	   Nothing -> (s, [])
	   Just ms' -> 
	        let c' = (i+1,j) in
		if mf ! xy then
		    let as = allBombs mf in
		    (Stop rs n, [Msg "You just exploded!", stat c' bs fs, Repaint [paintBomb bs a | a<-as], Repaint [(xy,SBombX) | xy<-bs, xy `notElem` as]])
		else 
		    if xy == mineFieldSize then
		        (Stop rs n, [Msg "You honestly made it!", stat c' bs fs, Repaint (map (paint mf ms' bs) (changes ms ms'))])
		    else
		        (Go rs n mf ms' c' bs fs, [Msg (mineMsg mf xy), stat c' bs fs, Repaint (map (paint mf ms' bs) (changes ms ms'))])
doMove s@(Go rs n mf ms@(Moves _ vis _) c bs fs) (Bomb xy) =
        if xy `elem` vis then
	    (s, [])
	else if xy `elem` bs then
	    let bs' = bs \\ [xy] in (Go rs n mf ms c bs' fs,          [stat c bs' fs, Repaint [(xy, SBlank)]])
	else
	    let bs' = xy:bs
                fs' = fs \\ [xy]
            in  (Go rs n mf ms c bs' fs', [stat c bs' fs', Repaint [(xy, SBomb)]])
doMove s@(Go rs n mf ms@(Moves _ vis _) c bs fs) (Free xy) =
        if xy `elem` vis then
	    (s, [])
	else if xy `elem` fs then
	    let fs' = fs \\ [xy] in (Go rs n mf ms c bs fs',          [stat c bs fs', Repaint [(xy, SBlank)]])
	else
	    let fs' = xy:fs
                bs' = bs \\ [xy]
            in  (Go rs n mf ms c bs' fs', [stat c bs' fs', Repaint [(xy, SFree)]])
doMove (Go rs n _ _ _ _ _) New = doMove (Stop rs n) New
doMove (Stop rs n) New =
	let (s@(Go _ _ a _ _ _ _), os) = doMove (newState rs n) (Move (1,1))
	in  (s, Repaint [((x,y), SBlank) | x<-[1..mineFieldSizeX], y<-[1..mineFieldSizeY]] : Repaint [((1,1), SCurrent (countBombs a (1,1)))] : stat (0,0) [] []:os)
doMove (Go rs _ mf ms c bs fs) (SetSize n) = (Go rs n mf ms c bs fs, [])
doMove (Go rs n mf ms@(Moves _ _ ps) (i,j) bs fs) Hint = 
        let m = case filter (\xy->not (mf ! xy)) ps of
                  [] -> Msg "Impossible"
		  xy:_ -> Repaint [(xy, SFree)]
            c' = (i,j+1)
        in  (Go rs n mf ms c' bs fs, [stat c' bs fs, m])
doMove (Stop rs _) (SetSize n) = (Stop rs n, [])
doMove s _ = (s, [])

stat :: Counters -> Bombs -> Bombs -> MOutput
stat (i, j) bs fs = Status ("Moves: "++show i++"   Hints: "++show j++"   Safes: "++show (length fs)++"   Mines: "++show (length bs))

mineMsg a xy =
	case countBombs a xy of
	   0 -> "No mines nearby."
	   1 -> "1 mine nearby."
	   n -> shows n " mines nearby."

changes :: Moves -> Moves -> [Coord]
changes (Moves cur vis bor) (Moves cur' vis' bor') =
	let chg l l' = union l l' \\ intersect l l'
	in  nub (chg [cur] [cur'] ++ chg vis vis' ++ chg bor bor')

paint :: MineField -> Moves -> Bombs -> Coord -> (Coord, Symbol)
paint a (Moves cur vis _) bs xy =
	(xy, 
	if xy == cur then
	    SCurrent (countBombs a xy)
	else if xy `elem` vis then
	    SNumber (countBombs a xy)
	else if xy `elem` bs then
	    SBomb
	else
	    SBlank)

countBombs a = length . filter id . map (a!) . neighbours

allBombs a = [(x,y) | x<-[1..mineFieldSizeX], y<-[1..mineFieldSizeY], a!(x,y)]

paintBomb bs xy =
	(xy,
	if xy `elem` bs then
		SBomb
	else
		SBombDone)

firstState :: StdGen -> (State, [MOutput])
firstState g =
	{-let r = truncate d
	in-}  doMove (newState g initMineCount) New

stripR = filter f where f (Repaint []) = False
                        f _ = True

doMoves :: State -> [MInput] -> [[MOutput]]
doMoves s [] = []
doMoves s (i:is) =
	let (s', os) = doMove s i
	in  stripR os : doMoves s' is

play :: StdGen -> [MInput] -> [[MOutput]]
play d is =
	let (s, os) = firstState d
	in  stripR os : doMoves s is


-- Stream Processor version of play & doMoves (hallgren 930525):

--doMovesSP :: State -> SP MInput MOutput
doMovesSP s =
	getSP $ \i ->
	let (s', os) = doMove s i
	in  putsSP (stripR os) $ doMovesSP s'

--playSP :: Double -> SP MInput MOutput
playSP d =
	let (s, os) = firstState d
	in  putsSP (stripR os) $ doMovesSP s
