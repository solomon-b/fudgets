module Diff(diff,diffCmd) where
import Exp
import Ops
import Simp

diffCmd cmd =
  case cmd of
    Fun e -> Fun (diff e)
    Approx a n e -> Approx a n (diff e)

diff e =
  case e of
     Var -> one
     Const _ -> zero'
     Pow b e -> ce e `mul` diff b `mul` pow b (e-1)
     Binop bop e1 e2 -> diffbinop bop e1 e2
     Unop uop e -> mul (diff e) (uopdiff uop e)


diffbinop bop e1 e2 =
  case bop of
     Badd -> add (diff e1) (diff e2)
     Bsub -> sub (diff e1) (diff e2)
     Bmul -> add (mul e1 (diff e2)) (mul (diff e1) e2)
     Bdiv -> div' (sub (mul (diff e1) e2) (mul e1 (diff e2))) (mul e2 e2)
     _ -> unimpl


uopdiff unop e =
  case unop of
     Uneg -> neg one
     Usin -> Unop Ucos e
     Ucos -> neg (Unop Usin e)
     Uexp -> Unop Uexp e
     Utan -> add one (mul (Unop Utan e) (Unop Utan e))
     Uln -> inv e
     Uabs -> Unop Usgn e
     _ -> unimpl


unimpl = error "Unimplemented diff\n"

