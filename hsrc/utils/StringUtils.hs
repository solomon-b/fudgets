module StringUtils where
--import Char(isAlpha,isAlphanum,isDigit)

expandTabs n = exp (n::Int)
  where
    exp k [] = []
    exp k ('\t':xs) = [' '|_<-[1..k]] ++ exp n xs
    exp k (x:xs) = x:exp (if k==1 then n else k-1) xs

rmBS "" = ""
rmBS (c:'\b':cs) = rmBS cs
rmBS (c:cs) = c:rmBS cs

wrapLine n xs =
    let first = take n xs
        rest = drop n xs
    in  first : (if null rest then [] else wrapLine n rest)
