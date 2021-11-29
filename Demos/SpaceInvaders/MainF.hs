module MainF where
import Fudgets
import AllFudgets(toggleButtonF')
import ScoreF

--{- 
mainF worldF = nameLayoutF layout spaceInvadersF
  where
    layout = vBoxNL' 2 [a s,w,hBoxNL [l,n,p]]
      where
        [w,n,p,l,s] = map leafNL ["w","n","p","l","s"]
	a = marginHVAlignNL 0 aRight aTop

    spaceInvadersF = outF>==<nameF "w" (worldF (Point 480 390))>==<inF

    inF = nameF "p" pauseF>+<nameF "n" (buttonF "New")
    pauseF = toggleButtonF' (setKeys [([Control],"p")]) "Pause"

    outF = nameF "l" livesF >+< nameF "s" scoreF

    livesF = displayF' (setBgColor "black" . setFgColor "white")
--}
{-
mainF worldF = spaceInvadersF
  where
    spaceInvadersF = vBoxF (outF>==<worldF (Point 480 390)>==<inF)

    inF = swapEither >^=< hBoxF (buttonF "New" >+< pauseF)
    pauseF = toggleButtonF' (setKeys [([Control],"p")]) "Pause"

    outF = hBoxF (livesF >+< scoreF)

    livesF = displayF' (setBgColor "black" . setFgColor "white")
--}
