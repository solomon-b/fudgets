module ToggleDrawGroupF(toggleDrawGroupCF, toggleDrawGroupDF) where

import Fudgets
import DrawingF

--toggleDrawCF :: size -> drawing -> F (Either Bool ChangeTile) Event
toggleDrawCF size init = buttonBorderF 5 (drawDisplayCF size init)

--toggleDrawGroupCF :: layout -> choices 
--	-> F (Int, (Either Bool ChangeTile)) (Int, Event)
toggleDrawGroupCF layout choices = 
	let t v = toggleDrawCF (Point 50 50) v
	    toggles = zip [0..((length choices) - 1)] (map t choices)
	in listLF layout toggles

--toggleDrawDF :: size -> drawing -> F (Either Bool ChangeTile) Tile
toggleDrawDF size init = buttonBorderF 5 (drawDisplayDF size init)

--toggleDrawGroupDF :: layout choices 
--	-> F (Int, (Either Bool ChangeTile)) (Int, Tile)
toggleDrawGroupDF layout choices = 
	let t v = toggleDrawDF (Point 50 50) v
	    toggles = zip [0..((length choices) - 1)] (map t choices)
	in listLF layout toggles
