module Color(tryAllocColor,tryAllocColorF,
	     allocColor,allocColorF,
	     allocColorPixel,allocColorPixelF,
             tryAllocNamedColor,tryAllocNamedColorF,
	     allocNamedColor,allocNamedColorF,
	     allocNamedColorPixel,allocNamedColorPixelF,
	     allocNamedColorDef,
	     allocNamedColorDefPixel,
	     queryColor,queryColorF
             ) where
import Command
import Event
--import Fudget
import Xrequest
import Xtypes
import Cont
--import NullF(F,K)
import StdIoUtil(echoStderrK)
--import ContinuationIO(stderr)

genTryAlloc cmd xr =
    let expected (ColorAllocated color) = Just color
        expected _ = Nothing
    in xr cmd expected

genTryAllocColor xr cmap rgb = genTryAlloc (AllocColor cmap rgb) xr
genTryAllocNamedColor xr cmap cname = genTryAlloc (AllocNamedColor cmap cname) xr

tryAllocColor x = genTryAllocColor xrequest x
tryAllocColorF = genTryAllocColor xrequestF

tryAllocNamedColor x = genTryAllocNamedColor xrequest x
tryAllocNamedColorF = genTryAllocNamedColor xrequestF

--allocNamedColorDef :: ColormapId -> ColorName -> ColorName -> Cont (K a b) Color
allocNamedColorDef cmap cname fallback = 
    tryGet (tryAllocNamedColor cmap cname)
	   (echoStderrK
		      ("Warning, cannot allocate background color \""++cname++
		      -- backround ??
		       "\", using \""++fallback++"\" instead.") .
	    allocNamedColor cmap fallback)

--allocNamedColorDefPixel :: ColormapId -> ColorName -> ColorName -> Cont (K a b) Pixel
allocNamedColorDefPixel cmap cname fallback = allocNamedColorDef cmap cname fallback
  . (.colorPixel)

safe req cmap c = tryM (req cmap c) (error ("Cannot allocate color: "++show c))

pixel :: (a->b->Cont c Color) -> a->b->Cont c Pixel
pixel req cmap c = req cmap c . (.colorPixel)

allocNamedColor x = safe tryAllocNamedColor x
allocNamedColorF = safe tryAllocNamedColorF

allocNamedColorPixel x = pixel allocNamedColor x
allocNamedColorPixelF = pixel allocNamedColorF

allocColor x = safe tryAllocColor x
allocColorF = safe tryAllocColorF

allocColorPixel x = pixel allocColor x
allocColorPixelF = pixel allocColorF

querycolor xr cmap pixel =
    let expected (ColorQueried c) = Just c
        expected _ = Nothing
    in xr (QueryColor cmap pixel) expected

queryColorF = querycolor xrequestF
queryColor x = querycolor xrequest x
