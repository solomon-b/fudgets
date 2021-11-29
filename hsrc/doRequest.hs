import Control.Monad(forever)
import System.IO
import DoRequest

main =
  do state <- initXCall
     forever (print' =<< doRequest state =<< readLn')

print' x = do --hPutStr stderr "OR: "
            --hPrint stderr x
              print x
              hFlush stdout

readLn' = do r <- readLn
           --hPutStr stderr "IR: "
           --hPrint stderr r
             return r
