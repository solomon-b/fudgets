module Show(show_Exp,show_Cmds,show_Cmd,showf,showp) where
import Exp
import Ops
import Numeric(showGFloat)
import Compat(mix)
--import Printf -- From hbc_library, comment out if not compiling with hbc.

--- hbc version:
--showf x = printf "%.5g" [UDouble x]
--showp (x,y) = printf "(%.5g,%.5g)" [UDouble x,UDouble y]
--- standard version:
--showf = show
showf x = showGFloat (Just 5) x ""
showp (x,y) = "("++show x++","++show y++")"
---

show_Cmds cmds = mix (map show_Cmd cmds) ", "

show_Cmd cmd =
  case cmd of
    Fun e -> show_Exp e
    Approx a n e -> "A "++show_Approx a++" "++show n++" "++show_Exp e

show_Approx Trapets = "t"
show_Approx RectO = "o"
show_Approx RectU = "u"

--show_Exps es = mix (map show_Exp es) ", "
show_Exp e =
  case e of
     Binop Badd e1 e2 -> show_Exp e1 ++ show_Bop Badd ++ show_Exp e2
     Binop bop e1 e2 | bop `elem` addopt -> show_Exp e1 ++ show_Bop bop ++ show_term e2
     Unop uop e | uop==Uneg -> show_Uop Uneg ++ show_term e
     _ -> show_term e

show_term e =
  case e of
     Binop bop e1 e2 | bop `elem` mulopt -> show_term e1 ++ show_Bop bop ++ show_power e2
     _ -> show_power e

show_power e =
  case e of
    Pow b n -> show_aexp b ++ "^" ++ show n
    _ -> show_aexp e

show_aexp e =
  case e of
     Var -> "x"
     Const x -> show x
     Unop uop e | uop/=Uneg -> show_Uop uop ++ show_argexp e
     e -> "(" ++ show_Exp e ++ ")"


show_argexp e =
  case show_aexp e of
     s @ ('(' : _) -> s
     s -> " " ++ s
