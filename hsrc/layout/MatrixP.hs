{-# LANGUAGE CPP #-}
module MatrixP(matrixP,matrixP') where
import Geometry
import LayoutRequest
import LayoutDir(LayoutDir(..),xc,yc,mkp)
import Spacers(Distance)
import Defaults(defaultSep)
import Data.List(mapAccumL)

#ifndef __HBC__
#define fromInt fromIntegral
#endif

matrixP n = matrixP' n Horizontal defaultSep

matrixP' :: Int -> LayoutDir -> Distance -> Placer
matrixP' count' ld sep = P $ \ requests ->
    let n = length requests
        maxsize = (pMax . (origin:) . map minsize) requests
	rps' = concatMap refpoints requests
        maxh = xc ld maxsize
        maxv = yc ld maxsize
        ncols = count' `min` n
        nrows = (n - 1) `quot` count' + 1
        his = sep * (ncols - 1)
        vis = sep * (nrows - 1)
        h = his + maxh * ncols
        v = vis + maxv * nrows
        mat2 (Rect gotpos gotsize) =
            let x0 = xc ld gotpos
	        y0 = yc ld gotpos
		width = xc ld gotsize
		height = yc ld gotsize
                goth,gotv :: Double
                goth = fromInt (width - his) / fromInt ncols
                gotv = fromInt (height - vis) / fromInt nrows
                pl i _ =
		  let (x, y) = (i `rem` count', i `quot` count')
		  in (i + 1, Rect
		   (mkp ld (x0 + truncate (fromInt x * (goth + fromInt sep)))
			   (y0 + truncate (fromInt y * (gotv + fromInt sep))))
		   (mkp ld (truncate goth)
			   (truncate gotv)))
            in  snd (mapAccumL pl 0 requests)
    in  (refpLayout (mkp ld h v) False False rps', mat2)

#ifdef __NHC__
fromInt = fromIntegral
#endif
