module Timer(timer) where

import Fudgets

timer :: Int -> F Bool Tick
timer t = timerF >=^< \b -> if b then Just (t,0) else Nothing
