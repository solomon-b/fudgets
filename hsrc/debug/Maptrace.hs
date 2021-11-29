module Maptrace(maptrace,ctrace) where
import Debug.Trace(trace)
import CmdLineEnv(argFlag)

maptrace s = map (\x -> if x == x then trace s x else error "?")

ctrace flag =
       if argFlag flag False
       then \e -> trace (flag++": "++show e)
       else const id
