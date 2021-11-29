{-# LANGUAGE CPP #-}
{- Obsolete OPTIONS -#include <X11/Xlib.h> -#include <X11/Xutil.h> -#include <X11/extensions/Xdbe.h> -fvia-C -}
--  -optc-I/usr/X11R6/include
module DoXRequest(doXRequest,getGCValues,translateCoordinates) where

--import P_IO_data(Request(..),Response(..))
import Geometry
import Command
import Event
import Xtypes
import Font
--import ResourceIds
import Visual
import HbcWord(intToWord) -- for Visual
import IOUtil(getEnvi)
--import CmdLineEnv(argFlag)

import XCallTypes
import StructFuns
import Xlib
import Marshall
import MyForeign -- debugging
import CString16

--import Ap

{-
#include "newstructfuns.h"
-}

newXID = newPtr :: IO CXID
newLong = newPtr :: IO CLong
newLongs n = sequence (replicate n newLong)
readLong = readCVar :: CLong -> IO Int
newInt32 = newPtr :: IO CInt32
newInt32s n = sequence (replicate n newInt32)
readInt32 = readCVar :: CInt32 -> IO Int32
freePtrs xs = mapM_ freePtr xs

--synchronize = argFlag "synchronize" False

{-
doXRequest req@(display,_,_) =
  do r <- doXRequest' req
     if synchronize && display/=noDisplay then xSync display False else return ()
     return r
-}
{-
doXRequest req@(display,_,_) | display/=noDisplay =
    withLockedDisplay display $ doXRequest' req
doXRequest req = doXRequest' req
-}
doXRequest (d@display,wi,req) =
 case req of
   OpenDisplay optname ->
      do name <- if null optname
		 then case getEnvi "DISPLAY" of
		        Just n -> return n
	                Nothing -> failu "DISPLAY variable is not set"
		 else return optname
         d <- xOpenDisplay name
	 --xSynchronize d synchronize -- has no effect?!
	 return (DisplayOpened d)
   CreateSimpleWindow p r -> WindowCreated <$> createWindow wi r
   CreateRootWindow r resname -> WindowCreated <$> do
       dr <- xDefaultRootWindow display
       this <- createWindow dr r
       setClassHint display this resname "Fudgets"
	  /* This is a hack that solves a focus problem with twm.
	     The twm option NoTitleFocus also solves the problem. */
       -- xSetWMHints display this InputHint True
       return this
   CreateGC dr oldgc gcattrs -> GCCreated <$> do
     wi' <- if wi == noWindow then xDefaultRootWindow display else return wi
     gcvals <- getGCValues gcattrs
     gc <- createGC display (getdrawable wi' dr)
     if oldgc == rootGC then do
        screen <- xDefaultScreen display
        xSetForeground display gc =<< xBlackPixel display screen
	xSetBackground display gc =<< xWhitePixel display screen
      else copyGC display oldgc gc
     changeGC display gc gcvals
     return gc
   LoadFont fn -> FontLoaded <$> xLoadFont display fn
   CreateFontCursor shp -> CursorCreated <$> xCreateFontCursor display shp
   GrabPointer b evm ->
     PointerGrabbed <$> xGrabPointer display wi b (toC evm)
	  grabModeAsync grabModeAsync windowNone cursorNone currentTime
   AllocNamedColor cm cn ->
      ColorAllocated <$> (allocNamedColor display cn =<< dcm cm)
   AllocColor cm rgb ->
       ColorAllocated <$> (allocColor display rgb =<< dcm cm)
   CreatePixmap (Point w h) depth -> PixmapCreated <$> do
     depth' <- if depth == copyFromParent
		  then xDefaultDepth display =<< xDefaultScreen display
		  else return depth
     WindowId wi' <- if wi == noWindow then xDefaultRootWindow display else return wi
     xCreatePixmap display (DrawableId wi') w h depth'
   ReadBitmapFile filename -> do
      bm <- newXID
      ints@[w,h,xhot,yhot] <- newInt32s 4
      WindowId root <- xDefaultRootWindow display
      cfilename <- marshallString filename -- GHC gives faulty code for marshallM?
      --putStrLn "about to call xReadBitmapFile"
      --writeCVar xhot 0
      --putStrLn filename
      --putStrLn =<< unmarshall cfilename
      --print =<< readLong xhot
      r <- xReadBitmapFile display (DrawableId root) cfilename w h bm xhot yhot
      --putStrLn "returned from call to xReadBitmapFile"
      freePtr cfilename -- crash?!!
      --putStrLn "about to read xhot"
      x <- fromIntegral <$> readInt32 xhot
      --putStr "xhot =";print x
      ret <-
        if (r::Int) == CCONST(BitmapSuccess) then do
	    hot <- if x == -1 then return Nothing 
			      else Just . Point x . fromIntegral <$> readInt32 yhot
	    BitmapReturn
	      <$> (Point <$> (fromIntegral <$> readInt32 w) <*> (fromIntegral <$> readInt32 h))
	     <*> return hot 
	     <*> (PixmapId <$> readCVar bm)
        else return BitmapBad
      --putStrLn "about to free int parameters"
      freePtrs ints
      freePtr bm
      return (BitmapRead ret)
   CreateBitmapFromData (BitmapData size@(Point w h) hot bytes) ->
     do cbytes <- marshallString' (map toEnum bytes) (((w+7) `div` 8)*h)
        WindowId wi' <- if wi == noWindow then xDefaultRootWindow display else return wi
        let cw = fromIntegral w
            ch = fromIntegral h
        pm <- xCreateBitmapFromData display (DrawableId wi') cbytes cw ch
        freePtr cbytes
        return (BitmapRead (BitmapReturn size hot pm))
   --RmGetStringDatabase str ->
   --RmGetResource rmd s1 s2 ->
   TranslateCoordinates ->
     do rootwin <- xDefaultRootWindow display
        CoordinatesTranslated . maybe origin id
          <$> translateCoordinates display wi rootwin
   InternAtom str b -> GotAtom <$> xInternAtom display str b

   GetAtomName (Atom a) -> 
     do at_ret <- xGetAtomName display a
        if (at_ret == nullStr)
           then return (GotAtomName Nothing)
           else
             do at_name <- unmarshallString at_ret
                xFree at_ret
                -- putStrLn at_name
                case (length at_name) of
                   0 -> return (GotAtomName Nothing)
                   _ -> return (GotAtomName $ Just at_name)

   GetWindowProperty offset property delete req_type ->
     do let length = 1000 
	actual_type <- newPtr
	actual_format <- newPtr
	nitems <- newLong
	bytes_after <- newLong
	prop_return <- newCString
	xGetWindowProperty display wi property offset
			   (length `div` (4::Int)) delete req_type
	       actual_type actual_format nitems bytes_after prop_return
	at <- readCVar actual_type
	af <- readCVar actual_format
	n <- readLong nitems
	ba <- readLong bytes_after
	str <- if (af::Int) == CCONST(None) then return "" else
	    do let got = af*n `div` 8
	       cstr <- readCVar prop_return
	       str <- unmarshallString' cstr got
	       xFree cstr
	       return str
	freePtr actual_type
	freePtr actual_format
	freePtr nitems
	freePtr bytes_after
	freePtr prop_return
	return $ GotWindowProperty at af n ba str
   QueryPointer -> do
     ints@[root,child,root_x,root_y,win_x,win_y,mask] <- newLongs 7
     same <- xQueryPointer display wi root child root_x root_y win_x win_y mask
     ret <- PointerQueried same
	      <$> mkPoint (readLong root_x) (readLong root_y)
	     <*> mkPoint (readLong win_x) (readLong win_y)
	     <*> (fromC <$> readLong mask)
     freePtrs ints
     return ret

   QueryFont fid -> FontQueried <$> queryFont display fid
   LoadQueryFont fn -> FontQueried <$> loadQueryFont display fn

   QueryColor cmid (Pixel px) -> do
      c <- newXColor
      SET(XColor,Word,c,pixel,px)
      cm <- dcm cmid
      xQueryColor display cm c
      r <- mkColor c
      freePtr c
      return (ColorQueried r)

   ListFonts pattern maxnames ->
     do cnt <- newLong
	fnarr <- xListFonts display pattern maxnames cnt
	fns <- unmarshallArray fnarr =<< readLong cnt
	xFreeFontNames fnarr
	freePtr cnt
	return (GotFontList fns)
   --QueryTree ->
   DefaultRootWindow -> GotDefaultRootWindow <$> xDefaultRootWindow display
   --GetGeometry ->
   --GetResource rms ->
   DefaultVisual ->
     GotVisual <$> (mkVisual =<< xDefaultVisual d =<< xDefaultScreen d)
   Sync b -> xSync display b >> return Synced
   QueryTextExtents16 fid s ->
     do let n = length s
	cs <- marshallString16' s n
        ints@[dir,ascent,descent,overall] <- newLongs 4
        overall <- newPtr
	xQueryTextExtents16 display fid cs n dir ascent descent overall
        [asc,desc] <- mapM readLong [ascent,descent]
        ov <- mkCharStruct overall
        freePtrs ints
	freePtr overall
        return $ TextExtents16Queried asc desc ov
   ListFontsWithInfo pattern maxnames ->
     GotFontListWithInfo <$> listFontsWithInfo d pattern maxnames
   DbeQueryExtension ->
     do ints@[major,minor] <- newLongs 2
        status <- xdbeQueryExtension display major minor
	ma <- readLong major
	mi <- readLong minor
	freePtrs ints
	return (DbeExtensionQueried status ma mi)
   DbeAllocateBackBufferName swapAction ->
       DbeBackBufferNameAllocated <$> xdbeAllocateBackBufferName d wi swapAction
   DbeSwapBuffers swapAction -> -- applies only to the fudget's own window.
       do (swapinfo,cnt) <- storeSwapAction [(wi,swapAction)]
          status <- xdbeSwapBuffers display swapinfo cnt
	  freePtr swapinfo
	  return (DbeBuffersSwapped status)
   _ -> error (notImplemented req)
 where
  createWindow parent (Rect (Point x y) (Point w h)) = do
    screen <- xDefaultScreen display
    blackP <- xBlackPixel display screen
    whiteP <- xWhitePixel display screen
    let border_width = 0 -- should agree with border_width in WindowF.hs !!
    this <- xCreateSimpleWindow display parent x y w h border_width blackP whiteP
    xStoreName display this "Fudgets"
    return this
  dcm = dcmap display

{-
type BorderWidth = Int
type FontShape = Int
-}

setClassHint :: Display -> Window -> String -> String -> IO ()
setClassHint d w resName resClass =
  do class_hints <- newPtr
     rn<-marshallString resName
     SET(XClassHint,CString,class_hints,res_name,rn)
     rc<-marshallString resClass
     SET(XClassHint,CString,class_hints,res_class,rc)
     xSetClassHint d w class_hints
     freePtr rn
     freePtr rc

createGC :: Display -> DrawableId -> IO GCId
createGC d w = xCreateGC d w 0 nullPtr
--    _casm_ ``%r=XCreateGC(%0,%1,0,NULL);'' d w

copyGC :: Display -> GCId -> GCId -> IO ()
copyGC d oldgc gc = 
  --xCopyGC d oldgc ``(1<<(GCLastBit+1))-1)''gc
  xCopyGC d oldgc 8388607  gc
--   _casm_ ``XCopyGC(%0,%1,(1<<(GCLastBit+1))-1,%2);'' d oldgc gc

changeGC d gc (gcvals,mask) = xChangeGC d gc mask gcvals

allocNamedColor :: Display -> String -> ColormapId -> IO (Maybe Color)
allocNamedColor d colname cm = do
   exact <- newPtr
   screen <- newPtr
   status <- xAllocNamedColor d cm colname screen exact
   r <- returnColor status screen
   freePtr exact
   freePtr screen
   return r

returnColor status c = 
 if status /= (0::Int)
    then Just <$> mkColor c
    else return Nothing

mkColor xcol = 
 Color . Pixel
  <$> GET(XColor,Word,xcol,pixel)
  <*> (RGB
	 <$> GET(XColor,Int,xcol,red)
	 <*> GET(XColor,Int,xcol,green)
	 <*> GET(XColor,Int,xcol,blue)
      )

allocColor :: Display -> RGB -> ColormapId -> IO (Maybe Color)
allocColor d rgb cm = do
  color <- newXColor
  setRGB rgb color
  status <- xAllocColor d cm color
  r <- returnColor status color
  freePtr color
  return r

setRGB (RGB red green blue) color = do
  SET(XColor,Int,color,red,red)
  SET(XColor,Int,color,green,green)
  SET(XColor,Int,color,blue,blue)

type XGCValuesMask = (CXGCValues,Bitmask)

getGCValues :: GCAttributeList -> IO XGCValuesMask
getGCValues = getValues newPtr getGCValue where
  getGCValue gcv ga = case ga of
    GCFunction f           -> (SET(XGCValues,Int,gcv,function,fromEnum f),CWORD32(GCFunction)::Bitmask)
    GCForeground p         -> (SET(XGCValues,Int,gcv,foreground,toC p),CWORD32(GCForeground))
    GCBackground p         -> (SET(XGCValues,Int,gcv,background,toC p),CWORD32(GCBackground))
    GCLineWidth w          -> (SET(XGCValues,Int,gcv,line_width,w),CWORD32(GCLineWidth))
    GCLineStyle s          -> (SET(XGCValues,Int,gcv,line_style,fromEnum s),CWORD32(GCLineStyle))
    GCFont f               -> (SET(XGCValues,XID,gcv,font,toXID f),CWORD32(GCFont))
    GCCapStyle s           -> (SET(XGCValues,Int,gcv,cap_style,fromEnum s),CWORD32(GCCapStyle))
    GCJoinStyle s          -> (SET(XGCValues,Int,gcv,join_style,fromEnum s),CWORD32(GCJoinStyle))
    GCSubwindowMode m      -> (SET(XGCValues,Int,gcv,subwindow_mode,fromEnum m),CWORD32(GCSubwindowMode))
    GCGraphicsExposures g  -> (SET(XGCValues,Int,gcv,graphics_exposures, fromEnum g),CWORD32(GCGraphicsExposures))
    GCFillStyle f          -> (SET(XGCValues,Int,gcv,fill_style,fromEnum f),CWORD32(GCFillStyle))
    GCTile p               -> (SET(XGCValues,XID,gcv,tile,toXID p),CWORD32(GCTile))
    GCStipple p            -> (SET(XGCValues,XID,gcv,stipple,toXID p),CWORD32(GCStipple) :: Bitmask)
    -- _ -> (return (),0)


loadQueryFont :: Display -> FontName -> IO (Maybe FontStructList)
loadQueryFont d fn = mkFontStructList =<< xLoadQueryFont d fn

queryFont :: Display -> FontId -> IO (Maybe FontStructList)
queryFont d fi = mkFontStructList =<< xQueryFont d fi

listFontsWithInfo :: Display -> FontName -> Int -> IO [(FontName,FontStructList)]
listFontsWithInfo d pattern maxnames =
  do cnt <- newInt32
     fsarrp <- newCXFontStruct
     --putStrLn "About to call xListFontsWithInfo"
     fnarr <- xListFontsWithInfo d pattern (fromIntegral maxnames) cnt fsarrp
     --putStrLn "Returned from call xListFontsWithInfo"
     n <- readInt32 cnt
     --putStr "Number of fonts: " ; print n
     freePtr cnt
     fsarr <- readCVar fsarrp
     freePtr fsarrp
     --putStrLn "After free fsarrp"
     if fnarr==nullStr then return []
      else do
       --putStrLn "Non-null name array"
       fns <- unmarshallArray fnarr (fromIntegral n)
       --putStrLn "Got name list";print fns
       fss <- readArray fsarr (fromIntegral n)
       --putStrLn "Got fontstruct list"
       xFreeFontInfo fnarr fsarr n
       --putStrLn "Freed fontinfo, returning"
       return (zip fns fss)

instance CVar CXFontStruct FontStructList
instance Storable FontStructList where -- just for readArray...
--sizeOf _ = sizeOf (undefined::CXFontStruct) -- Wrong size!!!
  sizeOf _ = SIZEOF(XFontStruct)
  alignment _ = alignment (undefined::CXFontStruct)
  peek = mkFontStructList' . CXFontStruct

mkFontStructList :: CXFontStruct -> IO (Maybe FontStructList)
mkFontStructList fs =
   if fs == CXFontStruct nullAddr
   then return Nothing
   else do fsl <- mkFontStructList' fs
           --_casm_ ``XFreeFontInfo(NULL, %0, 1); '' fs
	   xFreeFontInfo nullStr fs 1
	   return (Just fsl)

#if 0
#define DEBUG(cmd) (putStrLn "cmd before">>(cmd)>>= \r->putStrLn "after">>return r)
#else
#define DEBUG(cmd) (cmd)
#endif

mkFontStructList' :: CXFontStruct -> IO FontStructList
mkFontStructList' fs =
  do --putStrLn "Enter mkFontStructList'"
     min_char_or_byte2 <- GET(XFontStruct,Int,fs,min_char_or_byte2)
     min_byte1 <-GET(XFontStruct,Int,fs,min_byte1)
     max_char_or_byte2 <- GET(XFontStruct,Int,fs,max_char_or_byte2)
     max_byte1 <-GET(XFontStruct,Int,fs,max_byte1)
     n_prop <-GET(XFontStruct,Int,fs,n_properties)
     -- putStrLn $ "font properties: " ++ (show n_prop)
     --putStrLn "after min max"
     let min = min_char_or_byte2 + 256*min_byte1
         max = max_char_or_byte2 + 256*max_byte1
         arrsize = (max_char_or_byte2 - min_char_or_byte2 + 1) *
		   (max_byte1 - min_byte1 + 1)
         --arrsize = max - min  :: Int -- This is wrong!
     --print ("min max arrsize",min,max,arrsize)
     per_char <- GET(XFontStruct,HT(XCharStruct),fs,per_char)
     --putStrLn "after per_char"
     elem9 <- if per_char /= nullPtr -- CCONST(NULL) 
       then Just <$> mapM (\i -> INDEX(XCharStruct) per_char i >>= mkCharStruct) [0..arrsize-1] 
       else return Nothing
     --putStrLn "after elem9"
     f_prop <- GET(XFontStruct,HT(XFontProp),fs,properties)
     elemprop <- if f_prop /= nullPtr
       then mapM (\i -> INDEX(XFontProp) f_prop i >>= mkFontProp) [0..n_prop-1]
       else return []
     -- putStrLn $ show elemprop
     fsl <- FontStructList . FontId
	<$> DEBUG(GET(XFontStruct,XID,fs,fid))
	<*> fmap toEnum DEBUG(GET(XFontStruct,Int,fs,direction))
	<*> return (toEnum min)
	<*> return (toEnum max)
	<*> fmap toEnum DEBUG(GET(XFontStruct,Int,fs,all_chars_exist))
	<*> DEBUG(GET(XFontStruct,Char,fs,default_char))
	<*> return elemprop
	<*> (mkCharStruct =<< DEBUG(AGET(XFontStruct,HT(XCharStruct),fs,min_bounds)))
	<*> (mkCharStruct =<< DEBUG(AGET(XFontStruct,HT(XCharStruct),fs,max_bounds)))
	<*> return elem9
	<*> DEBUG(GET(XFontStruct,Int,fs,ascent))
	<*> DEBUG(GET(XFontStruct,Int,fs,descent))
     -- putStrLn $ "Returning from mkFontStructList'" ++ (show fsl)
     return fsl

-- Intermediate data for FontProp, containing just two integers:
-- atom values will (hopefully) be retrieved lazily.

mkFontProp :: CXFontProp -> IO FontProp

mkFontProp fp = 
  FontProp
   <$> GET(XFontProp,Atom,fp,name)
  <*> GET(XFontProp,Int,fp,card32)

mkCharStruct :: CXCharStruct -> IO CharStruct
mkCharStruct cs = 
    CharStruct
     <$> GET(XCharStruct,Int,cs,lbearing)
     <*> GET(XCharStruct,Int,cs,rbearing)
     <*> GET(XCharStruct,Int,cs,width)
     <*> GET(XCharStruct,Int,cs,ascent)
     <*> GET(XCharStruct,Int,cs,descent)

mkVisual :: CVisual -> IO Visual
mkVisual cv =
  Visual
   <$> GET(Visual,VisualID,cv,visualid)
   <*> fmap toEnum GET(Visual,Int,cv,class)
   <*> fmap intToWord GET(Visual,Int,cv,red_mask)
   <*> fmap intToWord GET(Visual,Int,cv,green_mask)
   <*> fmap intToWord GET(Visual,Int,cv,blue_mask)
   <*> GET(Visual,Int,cv,bits_per_rgb)
   <*> GET(Visual,Int,cv,map_entries)

storeSwapAction :: [(WindowId,SwapAction)] -> IO (CXdbeSwapInfoArray,Int)
storeSwapAction =
  getArray newXdbeSwapInfoArray
	   (\si (i,(wi,sa)) ->
              do SETI(XdbeSwapInfo,WindowId,si,i,swap_window,wi)
		 SETI(XdbeSwapInfo,Int,si,i,swap_action,fromEnum sa))

translateCoordinates display window dstwindow =
  do dx <- newPtr
     dy <- newPtr
     child <- newPtr
     ok <- xTranslateCoordinates display window dstwindow 0 0 dx dy child
     p <- if ok
          then Just <$> (Point <$> (fromEnum <$> readInt32 dx)
                               <*> (fromEnum <$> readInt32 dy))
          else return Nothing
     freePtrs [dx,dy]
     freePtr child
     return p

