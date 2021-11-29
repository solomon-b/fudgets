module Eval where
import Exp
import Ops
--import List(union)

eval e =
  case e of
     Var ->  id
     Const k -> const k
     Pow b e -> let bv = eval b
                in \ x -> bv x ^^ e
     Binop bop e1 e2 ->
       let f = binopfun bop
           v1 = eval e1
           v2 = eval e2
       in \x -> f (v1 x) (v2 x)
     Unop unop e ->
       let f = unopfun unop
       in f . eval e

{-
freevars e =
  case e of
     Var x -> [x]
     Const _ -> []
     Binop _ e1 e2 -> union (freevars e1) (freevars e2)
     Unop _ e -> freevars e
-}
