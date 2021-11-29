module PQueue where

newtype PQueue a b = PQueue [(a,b)] deriving Show

empty = PQueue []

insert (PQueue q) v@(p,a) = PQueue $ insert' q where
   insert' [] = [v]
   insert' q@(v'@(p',_):q') | p < p'    = v : q
			    | otherwise = v' : insert' q'

inspect (PQueue q) = case q of
	[] -> Nothing
	(x:xs) -> Just (x,PQueue xs)

remove (PQueue q) i = PQueue $ filter (\((_,(_,i')))->i'/=i) q
