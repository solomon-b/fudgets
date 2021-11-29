module Main where
-- A simple stop watch with a run/stop button and a reset button.
import Fudgets

main = fudlogue (shellF "Stop Watch" stopWatchF)

stopWatchF = 
  timeDispF >==< (counterSP 0 >^^=< idRightF (timerF >=^< timeprep))
   >==< (runF >+< resetF)
  -- >+<quitButtonF

timeprep True = Just (100,100)
timeprep False = Nothing

runF = toggleButtonF "Run"
resetF = buttonF "Reset"

timeDispF = intDispF  -- to be improved

counterSP no = getSP $ \msg -> let out n = putSP n $ counterSP n 
			       in case msg of
				    Left _ -> out (no+1)
				    Right _ -> out 0
