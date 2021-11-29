module DesignF(designF) where

import AllFudgets
import DrawingF
import MyUtils
import Tiles

data DesignState = Init | Start Point

designSize = 250
gridNo = 10

designF :: F Tile Tile
designF = marginHVAlignF 0 aCenter aCenter (loopLeftF ((throRight padF) >==< designControlF))

padF = (drawDisplayCKF (Point designSize designSize) gridlines,Above)
		>#+< updateBF

updateBF = buttonF "finished design -> current tile"

designControlF :: F (Either (Either Event Click) Tile) (Either (Either ChangeTile a) Tile)
designControlF = 
	let startstate = ([], Init)
	    step (c, s) m = 
		let grid = gridlines 
		    grow = magT dtRatio
		    toDesign = Left. Left. changeToT
		    toChoice = Right
		in case m of
			-- from the grid
			(Left (Left (ButtonEvent _ ap _ _ Pressed (Button b)))) -> (
				let p = snap ap 
				in case s of
					Init    -> ((c, Start p), [toDesign ((mark p)++(grow c))])
					Start f -> ((newDraw, Init), [toDesign (grow newDraw)])
						where newDraw = case b of
							3         -> delLine x1 y1 x2 y2 c
							otherwise -> (line x1 y1 x2 y2) ++ c
						      Point x1 y1 = scalePoint tdRatio f
						      Point x2 y2 = scalePoint tdRatio p )
			-- from the update button
			(Left (Right Click)) -> ((c, s), [toChoice c])
			-- from Choice
			(Right t) -> ((t, Init), [toDesign (grow t)])
	in absF (mapstateSP step startstate)

snap :: Point -> Point
snap p = sp
	where sp = Point sx sy
	      sx = ((px+halfGridSize) `div` gridSize) * gridSize
	      sy = ((py+halfGridSize) `div` gridSize) * gridSize
	      Point px py = p 

delLine _ _ _ _ [] = [] 
delLine x1 y1 x2 y2 (c:cs) = 
	let rest = delLine x1 y1 x2 y2 cs
	in case c of
		DrawLine (Line (Point x3 y3) (Point x4 y4)) -> 
			if ((x1==x3) && (y1==y3) && (x2==x4) && (y2==y4)) ||
			   ((x1==x4) && (y1==y4) && (x2==x3) && (y2==y3))
				then rest else c:rest
		otherwise -> c:rest

mark p = circle x y (designSize `div` 50)
	where Point x y = p

gridlines = concat [(line (l*gridSize) 0 (l*gridSize) designSize) ++
	       (line 0 (l*gridSize) designSize (l*gridSize)) | 
			l <- [1..(gridNo-1)]]

gridSize = designSize `div` gridNo
halfGridSize = gridSize `div` 2
tdRatio = (fromIntegral tileSize) / (fromIntegral designSize)
dtRatio = designSize `div` tileSize
