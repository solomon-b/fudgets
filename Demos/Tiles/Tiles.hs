module Tiles where

import AllFudgets
import MyUtils

type Tile = [DrawCommand]
type ChangeTile = (Tile -> Tile)
type ChangeTileNo = (BoardTileNo -> BoardTileNo)
data BoardTileNo = NoTile | TileNo Int

tileSize :: Int
tileSize = 50

-- functions ending in 'T' manipulate tiles
-- functions ending in 'TN' manipulate tile numbers
-- (setT defines the relationship between tiles and tile numbers)

changeToT t = (\x -> t)
changeToTN :: BoardTileNo -> (BoardTileNo -> BoardTileNo)
changeToTN t = (\x -> t)

sameT = (\t -> t)
sameTN :: BoardTileNo -> BoardTileNo
sameTN = (\t -> t)

rotateT :: Tile -> Tile
rotateT [] = []
rotateT (t:ts) = let rest = rotateT ts
		 in case t of
	DrawLine (Line (Point x1 y1) (Point x2 y2)) -> 
		DrawLine (Line (Point (tileSize-y1) x1) (Point (tileSize-y2) x2)):rest
rotateTN tn = case tn of
		NoTile    -> NoTile
		otherwise -> let TileNo n = tn
				 d = n `div` 4 in
			     TileNo (((n+1) `mod` 4) + (d*4))

mirrorVT [] = []
mirrorVT (t:ts) = let rest = mirrorVT ts
		 in case t of
	DrawLine (Line (Point x1 y1) (Point x2 y2)) -> 
		DrawLine (Line (Point (tileSize-x1) y1) (Point (tileSize-x2) y2)):rest
mirrorVTN tn = 
	      case tn of
		NoTile    -> NoTile
		otherwise -> let TileNo n = tn
				 d = n `div` 4
		 		 m = n `mod` 4 in 
			     TileNo (((d+1) `mod` 2)*4 + m)

mirrorHT [] = []
mirrorHT (t:ts) = let rest = mirrorHT ts
		 in case t of
	DrawLine (Line (Point x1 y1) (Point x2 y2)) -> 
		DrawLine (Line (Point x1 (tileSize-y1)) (Point x2 (tileSize-y2))):rest
mirrorHTN tn = case tn of
		NoTile    -> NoTile
		otherwise -> let TileNo n = tn
				 d = n `div` 4 in
			     TileNo (((d+1) `mod` 2)*4 + ((n+2) `mod` 4))

magT m [] = []
magT m (t:ts) = let rest = magT m ts
		 in case t of
	DrawLine (Line (Point x1 y1) (Point x2 y2)) -> 
		DrawLine (Line (Point (x1*m) (y1*m)) (Point (x2*m) (y2*m))):rest

setT :: Tile -> [Tile]
setT t = [t, rotateT t, (rotateT.rotateT) t, (rotateT.rotateT.rotateT) t,
	  mirrorVT t, (rotateT.mirrorVT) t, (rotateT.rotateT.mirrorVT) t,
	  (rotateT.rotateT.rotateT.mirrorVT) t]

tile 0 = (line 0 25 25 0) ++ (line 25 50 50 25)
tile 1 = (line 25 0 25 50) ++ (line 0 25 50 25)
tile 2 = (line 0 25 25 0) ++ (line 25 0 25 50) ++ (line 25 50 50 25)
tile 3 = (line 0 25 25 0) ++ (line 25 0 50 25) ++ (line 50 25 25 50) ++
                (line 25 50 0 25)
tileRotL = (line 10 25 10 10) ++ (line 10 10 40 10) ++ (line 40 10 40 25) ++
                (line 40 25 35 20) ++ (line 40 25 45 20)
tileRotR = mirrorVT tileRotL
tileMirH = (line 25 5 25 45) ++ (line 20 10 25 5) ++ (line 25 5 30 10) ++
                (line 20 40 25 45) ++ (line 25 45 30 40)
tileMirV = rotateT tileMirH
