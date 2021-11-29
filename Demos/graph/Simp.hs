module Simp where
import Exp
import Ops

default(Int)

add (Const x) (Const y) = Const (x + y)
add (Const x) e = add e (Const x)
 -- || add (Unop Uneg e) e2 = sub e2 e -- changes the order of terms, confusing
add e (Unop Uneg e2) = sub e e2
add (Binop Badd e1 e2) e3 = add e1 (add e2 e3)
add e (Binop Bmul (k @ (Const _)) e2) | e==e2 = mul (add k one) e
add (Binop Bmul (k1 @ (Const _)) e1) (Binop Bmul (k2 @ (Const _)) e2) | e1==e2 = mul (add k1 k2) e1
add e1 e2 = if e2==zero' then e1
		else if e1==e2 then mul two e1
		else Binop Badd e1 e2

sub (Const x) (Const y) = Const (x - y)
sub e1 (Unop Uneg e2) = add e1 e2
sub e1 e2 = if e2==zero' then e1
		else if e1==zero' then Unop Uneg e2
		else if e1==e2 then zero'
		else Binop Bsub e1 e2

mul (Const x) (Const y) = Const (x * y)
mul Var Var = pow Var 2
mul Var (Pow Var n) = Pow Var (n+1)
mul (Pow Var n) Var = Pow Var (n+1)
mul (Pow Var n) (Pow Var m) = Pow Var (n+m)
mul e (Const y) = mul (Const y) e
mul e1 (Unop Uneg e2) = neg (mul e1 e2)
mul (Unop Uneg e1) e2 = neg (mul e1 e2)
mul e1 (Binop Bmul e2 e3) = mul (mul e1 e2) e3
 -- || mul (k1 @ (Const _)) (Binop Bmul (k2 @ (Const _)) e) = mul (mul k1 k2) e
mul e1 e2 = if e1==one then e2
		else if e1==zero' then zero'
		else if e1==minusone then neg e2
		else Binop Bmul e1 e2

div' (Const x) (Const y) = Const (x / y)
div' e1 (Unop Uneg e2) = neg (div' e1 e2)
div' (Unop Uneg e1) e2 = neg (div' e1 e2)
div' (Binop Bmul (Const x) e) (Const y) = mul (Const (x/y)) e
div' e1 e2 = if e2==one then e1
		else Binop Bdiv e1 e2

pow b 0 | b/=zero'= one
pow b 1 = b
pow b e = if b==one then one else Pow b e

neg (Const x) = (Const (0.0 - x))
neg (Unop Uneg e) = e
neg e = Unop Uneg e

inv e = div' one e

zero' = Const 0.0
one = Const 1.0
two = Const 2.0
minusone = neg one
ce = Const . fromIntegral
