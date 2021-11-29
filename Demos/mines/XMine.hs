module Main where
--import Trace
--import Word
import Fudgets
import FUtil
import MineField
--import BitMaps
--import Time
import System.Random(newStdGen)
import Box

main = newStdGen >>= fudlogue . shellF "FXMines v1.2" . xmines

--getTimeDouble = randomRIO (0,2e9)
                  

--myFont = defaultFont
--separation = 5::Int

xmines d =
  let title = nullF -- untaggedListLF horizontalP [labelF "FXMines v1.2"]
      controls = untaggedListLF horizontalP [labelF "Mine count", inp initMineCount, bf "Hint" Hint, bf "Restart" New{-, bq "Quit"-}]
      msg1 = disp ""
      msg2 = disp ""
      panel = untaggedListLF verticalP [ignoreI title,  		-- 0
                                        ignoreI controls, 		-- 1
                                        msg1 >=^< (\(Status s)->s),	-- 2
                                        msg2 >=^< (\(Msg s)->s), 	-- 3
                                        boxes' >=^< (\(Repaint l)->l)] >=^< directMsg -- 4
                             where directMsg m@(Repaint l) = (4, m)
                                   directMsg m@(Msg s)     = (3, m)
                                   directMsg m@(Status s)  = (2, m)
      but s = buttonF s
      bf s f = (\_ -> f) >^=< but s
      bq s = quitF >==< but s
      disp s = displayF
      inp n = (\x->case x of { InputChange n -> SetSize n; _ -> Noop} ) >^=< ignoreI (intWF 8 n)
      (xn, yn) = mineFieldSize
      boxes = mapstateSP action Move >^^=< vBoxF (actionF >+< boxes1) >=^< Right
        where
          action _ (Left (Fun f)  ) = (f,[])
          action f (Right (Move p)) = (f,[f p])
          action f (Right m       ) = (f,[m])

      actionF =
        radioGroupF' (setPlacer (spacerP centerS horizontalP) .
                      setLabelInside True)
                     [(Fun Move,"Move"),(Fun Free,"ok"),(Fun Bomb,"Bomb")]
                     (Fun Move)

      boxes1 :: F [(Coord, Symbol)] MInput
      boxes1 = readAllBitmaps $ \ ps ->
              (\(a,f)->f a) >^=< listLF (matrixP xn) [((x,y), boxF ps) | x<-[1..xn], y<-[1..yn]] >=^^< concatSP
      boxes' = --allcacheF
               boxes
  in  loopF ({-debugF show show-} panel >==< absF (playSP d))

data Fun = Fun (Coord->MInput)
instance Eq Fun where
  Fun f1 == Fun f2 = f1 (0,0) == f2 (0,0)
