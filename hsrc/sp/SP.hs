module SP where
import ShowFun() -- show instance for function types

data SP a b = PutSP b (SP a b) |
              GetSP (a -> SP a b) |
              NullSP 
          deriving (Show)

