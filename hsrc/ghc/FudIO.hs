module FudIO(fudIO1) where
import CmdLineEnv(argFlag)
--import Fudget
import SpIO
import TagEvents
import FudVersion

--fudIO1 :: F a b -> IO ()
fudIO1 mainF =
    if argFlag "version" False
    then showVersion
    else spIO mainSP
  where
    mainSP = tagEventsSP mainF

    showVersion = putStrLn ("Fudget library " ++ version)
