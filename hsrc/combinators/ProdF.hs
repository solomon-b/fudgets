module ProdF where
import CompOps((>+<), (>=^^<))
import Fudget
import SpEither(splitSP)

prodF :: (F a b) -> (F c d) -> F (a, c) (Either b d)
prodF leftw rightw = (leftw >+< rightw) >=^^< splitSP

