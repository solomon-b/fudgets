module RadioDrawF(radioDrawCF, radioDrawDF) where

import AllFudgets
import ToggleDrawGroupF
import Tiles
import MyUtils

radioDrawCF l c = loopLeftF (throRight (toggleDrawGroupCF l c) 
				>==< radioControlCF)

radioControlCF :: F (Either (Int, Event) (Int, Tile)) 
			(Either (Int, (Either Bool ChangeTile)) (Int, Event))
radioControlCF = 
	let outsideC n e = High (Right (n, e))
	    toggleButC n b = High (Left (n, Left b))
	    changeButC n d = High (Left (n, Right (changeToT d)))
	    startStateC = 0
	    press = ButtonEvent 0 (Point 0 0) (Point 0 0) [] Pressed (Button 1)
	    firstMsgC = [toggleButC 0 True, outsideC 0 press]
	    stepC c m = case m of
		High (Left (n, b)) -> (n, [toggleButC c False, 
						toggleButC n True, 
						outsideC n b])
		High (Right (n, t)) -> (c, [changeButC n t, outsideC c press])
	in putMessagesF firstMsgC $ F $ mapstateSP stepC startStateC


radioDrawDF l c = loopLeftF (throRight (toggleDrawGroupDF l c) 
				>==< radioControlDF (head c))

radioControlDF :: Tile -> F (Either (Int, Tile) (Int, Tile)) 
			(Either (Int, (Either Bool ChangeTile)) (Int, Tile))
radioControlDF t0 = 
	let outsideD n t = High (Right (n, t))
	    toggleButD n b = High (Left (n, Left b))
	    changeButD n d = High (Left (n, Right (changeToT d)))
	    startStateD = 0
	    firstMsgD = [toggleButD startStateD True,
			 outsideD startStateD t0]
	    stepD c m = case m of
		High (Left (n, t)) -> (n, [toggleButD c False, 
					toggleButD n True, outsideD n t])
		High (Right (n, t)) -> (c, [changeButD n t])
	in putMessagesF (firstMsgD) $ F $ mapstateSP stepD startStateD
