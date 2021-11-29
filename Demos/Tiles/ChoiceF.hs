module ChoiceF(choiceF) where

import Fudgets
import RadioDrawF
import Tiles
import MyUtils

choiceF = marginHVAlignF 0 aCenter aCenter (noStretchF True True choice')
choice' = loopLeftF (throRight (radioDrawDF layout choices) >==< choiceControlF)

layout = verticalP

choices = [tile n | n <- [0..3]]

choiceControlF :: F (Either (Int, Tile) Tile) 
			(Either (Int, Tile) (Either Tile (Either [Tile] [Tile])))
choiceControlF =
	let startstate = 0
	    step c m = 
		let toButtons = Left
		    toDesign = Right. Left
		    toTools = Right. Right. Left
		    toBoard = Right. Right. Right
		in case m of
			-- from Buttons
			(Left (n, t)) -> (n, [toDesign t, toTools (setT t), 
							toBoard (setT t)])
			-- from Design
			(Right t) -> (c, [toButtons (c, t), toTools (setT t),
							toBoard (setT t)])
	in absF (mapstateSP step startstate)
