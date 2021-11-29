module Main(main) where

import Fudgets
import BoardF
import ToolsF
import ChoiceF
import Tiles
import DesignF
import MyUtils

main = fudlogue (shellF "Escher" escherF)

escherF = placerF (revP (horizontalP' 10)) (playF >==< workF)

playF :: F (Either [Tile] [Tile]) c
playF = vBoxF (boardF >==< throRight toolsF)

workF :: F b (Either [Tile] [Tile])
workF = loopLeftF (hBoxF (worka >==< workb))

workb :: F (Either Tile c) (Either Tile (Either [Tile] [Tile]))
workb = choiceF >=^< fromLeft

worka :: F (Either Tile a) (Either Tile a)
worka = throRight (designF)

{--- older versions: ---

escherF = (playF >==#< (10, LRightOf, workF)) >==#< (10, LRightOf, quitButtonF)

playF = (boardF,Above) >#==< throRight toolsF

workF = loopLeftF ((worka,LeftOf) >#==< workb)

fromLeft :: Either a b -> a
fromLeft (Left x) = x

--}
