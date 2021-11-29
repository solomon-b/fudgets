--
-- Library with LML compatible functions.
--
module Compat where
import Data.Char(isAlpha,isDigit)

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d

takeWord :: String -> (String, String)
takeWord [] = ([],[])
takeWord (' ':cs) = takeWord cs
takeWord ('\n':cs) = takeWord cs
takeWord ('\t':cs) = takeWord cs
takeWord (c:cs) | isAlpha c = let (w,cs') = span (\c->isAlpha c || isDigit c || c=='_') cs in (c:w,cs')
		| isDigit c = let (w,cs') = span isDigit cs in (c:w, cs')
		| otherwise = ([c], cs)
