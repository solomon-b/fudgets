module Defaults(look3d, new3d, edgeWidth, defaultSep,
		paperColor, fgColor, bgColor, inputFg, inputBg,
		shadowColor, shineColor,
		defaultPosition, defaultSize, defaultFont, menuFont,
                buttonFont,labelFont) where
import Geometry(pP)
--import ListUtil(chopList,breakAt)
import Utils(segments)
import ResourceIds(FontName(..),ColorName(..))
import CmdLineEnv

argFont = argKey :: ( String -> FontName -> FontName)
argColor = argKey :: (String -> ColorName -> ColorName)

buttonFont  = argFont "buttonfont" labelFont
menuFont    = argFont "menufont"   labelFont
labelFont   = argFont "labelfont"  "variable"
defaultFont = argFont "font"       "fixed"

shineColor  = argColor "shine"   (if look3d then "white" else "lightgrey")
shadowColor = argColor "shadow"  (if look3d
                                  then if new3d
				       then "grey45"
				       else "black"
				  else "grey30")
paperColor  = argColor "paper"   "white"
inputFg     = argColor "inputfg" fgColor
inputBg     = argColor "inputbg" paperColor
fgColor     = argColor "fg"      "black"
bgColor     = argColor "bg"      "grey"

--defaultSep :: Int
defaultSep :: (Num a) => a
defaultSep = fromIntegral (argReadKey "sep" 5::Int)

defaultPosition =
    case segments (/='+') (argKey "geometry" "") of
      [_, x, y] -> Just (pP (read x) (read y))
      _ -> Nothing

defaultSize =
    case segments (/='x') (takeWhile (/='+') (argKey "geometry" "")) of
      [x, y] -> Just (pP (read x) (read y))
      _ -> Nothing

edgeWidth :: Int
edgeWidth = argReadKey "edgew" (if look3d then 2 else 4)

look3d = argFlag "look3d" True
new3d = argFlag "new3d" True
