
import System.Random(newStdGen)
import Fudgets
import WorldF
import MainF

main = do g <- newStdGen
          fudlogue (shellF "Space Invaders" (mainF (worldF g)))
