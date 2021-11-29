module ToolsF(toolsF) where

import Fudgets
import DrawingF
import RadioDrawF
import Tiles
import MyUtils

toolsF = loopLeftF ((throRight (radioDrawCF layout choices)) >==< toolControlF)

layout = matrixP 6 --matrixL 6 Horizontal 5

choices = [[], [], [], []] ++ [tileRotL] ++ [tileRotR] ++
	[[], [], [], []] ++ [tileMirH] ++ [tileMirV]

--toolControlF :: F (Either (Int, XEvent) [Tile]) (Either (Int, Tile) ChangeTileNo)
toolControlF = 
	let toButtons = Left
	    toBoard = Right
	    startstate = 0
	    step c m = case m of
		-- from Buttons
		(Left (n, _)) -> (n, [toBoard (t n)])
		-- from Choice
		(Right nt) -> (c, [toButtons (0, nt!!0),
					toButtons (1, nt!!1),
					toButtons (2, nt!!2),
					toButtons (3, nt!!3),
					toButtons (6, nt!!4),
					toButtons (7, nt!!5),
					toButtons (8, nt!!6),
					toButtons (9, nt!!7)])
	in absF (mapstateSP step startstate)

t 0  = changeToTN (TileNo 0)
t 1  = changeToTN (TileNo 1)
t 2  = changeToTN (TileNo 2)
t 3  = changeToTN (TileNo 3)
t 4  = rotateTN
t 5  = rotateTN . rotateTN . rotateTN
t 6  = changeToTN (TileNo 4)
t 7  = changeToTN (TileNo 5)
t 8  = changeToTN (TileNo 6)
t 9  = changeToTN (TileNo 7)
t 10 = mirrorHTN
t 11 = mirrorVTN
