module BoardF(boardF) where

import AllFudgets
import DrawingF
import Tiles
import MyUtils

boardF :: F (Either ChangeTileNo [Tile]) a
boardF = marginHVAlignF 0 aCenter aCenter (noStretchF True True (loopLeftF ((Left >^=< gridUF) >==< gridControlF)))

--left :: a -> Either a b
--left x = Left x

gridUF = (gridF,Above) >#+< updateF

updateF = buttonF "update with new current tile"

gridControlF :: F (Either (Either (Int, Event) Click) (Either ChangeTileNo [Tile])) 
			(Either (Int, ChangeTile) a)
gridControlF =
	let startState = (sameTN, [[]|x<-[0..7]], [NoTile|x<-[0..24]])
	    step (cn, ts, ns) m = 
		-- cn = function to change a tile number
		-- ts = set of rotations/reflections of current tile
		-- ns = tile numbers associated with each grid position
		let toGrid = Left
		in case m of
		-- looped back from Grid
		    (Left (Left (n, _))) -> ((cn, ts, nns), ct nn)
			where nn = cn (ns!!n)
			      nns = (take n ns) ++ [nn] ++ (drop (n+1) ns)
			      ct bt = case bt of
				NoTile   -> []
				TileNo t -> [toGrid (n,changeToT (ts!!t))]
		-- looped back from Update button
		    (Left (Right Click)) -> ((cn, ts, ns), changeAll)
			where changeAll = [ct n | n <- [0..24]]
			      ct n = case (ns!!n) of
				NoTile -> toGrid (n, sameT)
				TileNo t -> toGrid (n, changeToT (ts!!t))
		-- from Tools
		    (Right (Left ncn)) -> ((ncn, ts, ns), [])
		-- from Choice
		    (Right (Right nts)) -> ((cn, nts, ns), [])
	in absF (mapstateSP step startState)

gridF :: F (Int, ChangeTile) (Int, Event)
gridF = 
	let b v = drawDisplayCF (Point 50 50) v
	    tiles = [ (x, b []) | x <- [0..24] ]
	    layout = matrixP' 5 Horizontal 0
	in listLF layout tiles

