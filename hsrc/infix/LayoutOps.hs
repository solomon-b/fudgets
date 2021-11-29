module LayoutOps((>#==<), (>#+<)) where
--import Fudget
--import LayoutDir(Orientation)
import LayoutF(compLF,serCompLF)

infixl >#+<, >#==<

-- Infix operators for common layout combinators.

f1o >#+<  f2 = compLF f1o f2
f1o >#==< f2 = serCompLF f1o f2
