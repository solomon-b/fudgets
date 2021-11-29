module Convgc(convGCattrsK) where
import Color
--import Font(FontStruct)
--import Fudget
import LoadFont
--import Spops
import Xtypes
import EitherUtils() -- synonym Cont, for hbc

convGCattrsK attrs = gcattrsK attrs []

gcattrsK [] outattrs dr = dr (reverse outattrs)
gcattrsK (attr : attrs) outattrs dr =
  let cp attr' = gcattrsK attrs (attr' : outattrs) dr
  in case attr of
       GCForeground colname ->
         allocNamedColorPixel defaultColormap colname $ \fg ->
	 gcattrsK attrs (GCForeground fg : outattrs) dr
       GCBackground colname ->
         allocNamedColorPixel defaultColormap colname $ \fg ->
	 gcattrsK attrs (GCBackground fg : outattrs) dr
       GCFont fname ->
         loadFont fname $ \font ->
	 gcattrsK attrs (GCFont font : outattrs) dr
       GCFunction f -> cp (GCFunction f)
       GCLineWidth w -> cp (GCLineWidth w)
       GCLineStyle s -> cp (GCLineStyle s)
       GCCapStyle s -> cp (GCCapStyle s)
       GCJoinStyle s -> cp (GCJoinStyle s)
       GCSubwindowMode m -> cp (GCSubwindowMode m)
       GCGraphicsExposures b -> cp (GCGraphicsExposures b)

