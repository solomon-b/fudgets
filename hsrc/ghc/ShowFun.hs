module ShowFun where
import Text.Show.Functions()

{-
In Haskell 98, the function type is no longer an instance of the Show class.
This means that you can't derive Show for data types containing functions...
-}

instance Read (a->b)
