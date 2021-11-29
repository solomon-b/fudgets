module Parser(parse_Exp,expr,cmds,complete) where
import Parsop
import Exp
import Ops
import Compat(mix)
import Data.Char

cmds = seq' cmd (lit ",")

cmd
    = (lit "A" +<+ approx +++ int +++ expr ==> approxcmd)
  ||| (expr ==> Fun)
  where
    approxcmd ((a,n),e) = Approx a n e

approx
    = (lit "t" ==> const Trapets)
  ||| (lit "u" ==> const RectU)
  ||| (lit "o" ==> const RectO)

parse_Exp = complete . expr

--exprs = seq' expr (lit ",")
expr = leftAssoc term addop

term = leftAssoc factor mulop

factor = (aexp +++ optexponent) ==> (%%) (flip id)

aexp
    = num
  ||| var 
  ||| ((unop +++ factor) ==> (%%) Unop)
  ||| (lit "(" +<+ expr +>+ lit ")")

mulop = is (\t->elem t mulops) ==> binoplex
addop = is (\t->elem t addops) ==> binoplex
unop = is (\t->elem t unops) ==> unoplex
--ident = is iscellid ==> stoid
var = lit "x" ==> const Var
num = is (isDigit . head) ==> (Const . readf)

optexponent = cond exponent id
  where
     exponent = (lit "^" +<+ int) ==> (flip Pow)

int = is (all isDigit) ==> read

leftAssoc item op = item +++ (***) (op +++ item) ==> (%%) tree

tree e1 [] = e1
tree e1 ((op,e2):oes) = tree (Binop op e1 e2) oes

iscellid (c1:cs) = cs/=[] && isAlpha c1 && all isDigit cs

readf :: String -> Double
readf s = if '.' `elem` s then read s else read (s++".0")

complete ts =
  case ts of
     Right(e,[])->Right e
     Left ts-> Left ("Syntax error before: " ++ mix (take 10 ts) " ")
     Right(_,ts)-> Left ("Trailing garbage: " ++ mix (take 10 ts) " ")

