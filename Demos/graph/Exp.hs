module Exp where
import Ops

data Cmd
  = Fun Exp
  | Approx Approx Int Exp

data Approx = Trapets | RectU | RectO

--type Id = (Char, Int)

type Value = Double

data Exp
  = Var
  | Const Value
  | Pow Exp Int
  | Binop Bop Exp Exp
  | Unop Uop Exp
  deriving (Eq)

{-
stoid :: String -> Id
stoid (a:ds) = (a,read ds)

idtos :: Id -> String
idtos (c,n) = c:show n

-}
