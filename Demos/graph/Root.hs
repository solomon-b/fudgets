module Root where
import Eval
import Diff
import Exp
import Ops(Bop,Uop)

root e x0 dx =
  let f = eval e
      f' = eval (diff e)
      step x =  let y'=f' x
		in if y'==0.0
		   then Nothing
		   else Just (x - f x / y')
      infapproxs x = x:(case step x of
		          Nothing -> []
		          Just x2 -> infapproxs x2)
      approxs = take 20 (infapproxs x0)
      root (x1:x2:xs) = if abs (x1-x2)<dx
			then Just x2
			else root (x2:xs)
      root _ = Nothing

  in root approxs
