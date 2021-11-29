module Lex where
import HbcUtils(chopList,breakAt)
import Compat(takeWord)
import Data.Char

lex' = lexfix . filter (/=[]) . chopList taketoken

lexfix [] = []
lexfix (a:".":b:xs) = if isDigit (head a) && isDigit (head b)
 			  then (a++"."++b):lexfix xs
			  else a:".":lexfix (b:xs)
lexfix (x:xs) = x:lexfix xs

-- taketoken = takeword

taketoken [] = ([],[])
taketoken ccs@(c:cs) =
	case c of
	  '\"' -> let (s,rest) = breakAt '"' cs
		  in (c:s,rest)
	  _ -> takeWord ccs


