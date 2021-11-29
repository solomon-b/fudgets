module KeyGfx(keyGfx) where
import Fudgets
import Data.Char(toUpper)

keyGfx gfx k = hboxD' 12 [g gfx,keyD (keyCap k)]
  where
    keyD c = spacedD (hAlignS aRight `compS` hMarginS 0 3) $
	     hboxD' 0 [diamondD,spacedD alignKeysS $ g c]
    diamondD = spacedD vCenterS $ g $ FlexD 10 False False d
    d (Rect p (Point w h)) = [FillPolygon Convex CoordModeOrigin
		      [p+pP w2 0,p+pP w h2,p+pP w2 h,p+pP 0 h2]]
      where w2 = w `div` 2
	    h2 = h `div` 2

alignKeysS = minSizeS (pP 12 10) `compS` hCenterS
  -- This is a hack to make the keybord shortcuts align nicely in a column.
  -- It assumes that no character is wider than 12 pixels.

--ctrlkey = argFlag "menuctrlkey" False

keyCap k =
  case k of
    [c] -> [toUpper c]
    "period" -> "."
    "comma" -> ","
    "plus" -> "+"
    "minus" -> "-"
    "slash" -> "/"
    "asterisk" -> "*"
    "apostrophe" -> "'"
    "question" -> "?"
    "less" -> "<"
    "greater" -> ">"
    _ -> k -- ??
