{-# LANGUAGE CPP #-}
{- unused options -optc-I/usr/X11R6/include -#include <X11/Xlib.h> -#include <X11/Xutil.h> -fvia-C -}
-- -O
module DoXCommand(doXCommand) where

import Command
import Event
import Geometry
import Xtypes
--import Font
--import ResourceIds
import DrawTypes
import HbcUtils(chopList)

import DoXRequest(getGCValues,translateCoordinates)
import XCallTypes
import StructFuns
import Xlib
import Marshall
import MyForeign
--import CCall
import CString16

import System.IO(hPutStr,hPutStrLn,stderr) -- for error reporting

import PackedString(lengthPS,unpackPS)
--import Word(Word32)
import Data.Bits

--import IO(hFlush,stdout) -- debugging

default (Int)

{-
#include "newstructfuns.h"
-}
-- For debugging only:
{-
doXCommand req@(d,_,_) =
  do r <- doXCommand' req
   --hFlush stdout
     xSync d False
     return r
--}
--doXCommand cmd@(display,_,_) = withLockedDisplay display $ doXCommand' cmd

doXCommand :: (Display, Window, XCommand) -> IO ()
doXCommand (d,w,req) =
  case req of
   CloseDisplay d -> xCloseDisplay d
   DestroyWindow -> xDestroyWindow d w
   MapRaised -> xMapRaised d w
   LowerWindow -> xLowerWindow d w
   UnmapWindow -> xUnmapWindow d w
   ClearWindow -> xClearWindow d w
   StoreName s -> xStoreName d w s
   FreeGC gc -> xFreeGC d gc
   UngrabPointer -> xUngrabPointer d currentTime
   Flush -> xFlush d
   ClearArea (Rect (Point x y) (Point wi he)) exposures ->
      xClearArea d w x y wi he exposures
   GrabButton oe button ms evm ->
      xGrabButton d (toC button) anyModifier w oe (toC evm) grabModeAsync grabModeAsync windowNone  cursorNone
   UngrabButton b ms ->
     --_casm_ ``XUngrabButton(%0,%1,AnyModifier,%2);'' d (toC b) w
     xUngrabButton d (toC b) anyModifier w
   FreePixmap p -> xFreePixmap d p
   Draw drawable gc cmd -> doDrawCommand (getdr drawable) gc cmd
   DrawMany drawable gccmds -> mapM_ doDrawMany gccmds
      where
        dr = getdr drawable
	doDrawMany (gc,cmds) = mapM_ (doDrawCommand dr gc) cmds
   ChangeGC gc gcattrs -> do
     (gcvals,mask) <- getGCValues gcattrs
     xChangeGC d gc mask gcvals
   ChangeWindowAttributes was -> do
     (attrs,mask) <- getWindowAttributes was
     --putStr "CWA mask ";print mask
     xChangeWindowAttributes d w mask attrs
   ConfigureWindow wc -> do
     (chgs,mask) <- getWindowChanges wc
     xConfigureWindow d w mask chgs
   SetNormalHints (Point x y) -> do
      h <- newXSizeHints 
      SET(XSizeHints,Int,h,x,x)
      SET(XSizeHints,Int,h,y,y)
      SET(XSizeHints,Int,h,flags,CCONST(USPosition)::Int)
      xSetNormalHints d w h
      freePtr h
   SetWMHints i -> do
      h <- newXWMHints
      SET(XWMHints,Int,h,flags,CCONST(InputHint)::Int)
      SET(XWMHints,Int,h,input,toC i)
      xSetWMHints d w h
      freePtr h
   ShapeCombineMask kind (Point x y) p op ->
     xShapeCombineMask d w kind x y p op
   ShapeCombineRectangles kind (Point x y) rs op ord -> do
      (rsa,size) <- storeRectangles rs
      xShapeCombineRectangles d w kind x y rsa size op ord
      freePtr rsa
   ShapeCombineShape dst (Point x y)  p src op ->
       xShapeCombineShape d w dst x y p src op
   --RmDestroyDatabase RmDatabase |
   --RmCombineDatabase RmDatabase RmDatabase Bool |
   --RmPutLineResource RmDatabase String |
   SetWMProtocols atoms -> xSetWMProtocols d w atoms (length atoms)
   SendEvent dst propagate evm e -> do
     xe <- getEvent dst e
     status <- xSendEvent d dst propagate (fromIntegral $ toC evm) xe
     return ()
   SetSelectionOwner getit sel ->
     xSetSelectionOwner d sel (if getit then w else windowNone)  currentTime
   ConvertSelection  (Selection s t p) ->
     xConvertSelection d s t p w currentTime
   ChangeProperty w p t form@8 mode s -> -- !!
     xChangeProperty d w p t form mode s (length s)
   FreeColors cm pixls planes ->
     do cmid <- dcmap d cm
	(pxarr,size) <- toArray [fromIntegral p|Pixel p<-pixls] -- hmm
	xFreeColors d cmid pxarr size planes
   ReparentWindow newParent0 ->
     do newParent <- if newParent0==rootWindow
                     then xDefaultRootWindow d
                     else return newParent0
        mp <- translateCoordinates d w newParent
        case mp of
          Nothing -> hPutStr stderr "XTranslateCoordinates: windows on different screens!\n"
          Just (Point x y) -> xReparentWindow d w newParent x y
   --WarpPointer Point |
   SetRegion gc r -> -- !!! modifies a GC -- cache problems!
     do (rsa,_) <- storeRectangles [r]
	r <- xCreateRegion
	xUnionRectWithRegion rsa r r
	xSetRegion d gc r
	xDestroyRegion r
	freePtr rsa
   --AddToSaveSet |
   --RemoveFromSaveSet 
   Bell n -> xBell d n

   _ -> hPutStr stderr (notImplemented req)
 where getdr = getdrawable w

       doDrawCommand drw gc cmd = case cmd of
	  DrawLine (Line (Point x1 y1) (Point x2 y2)) -> 
	    xDrawLine d drw gc x1 y1 x2 y2
	  DrawImageString (Point x y) s ->
	    xDrawImageString d drw gc x y s (length s)
	  DrawString (Point x y) s ->
	    xDrawString d drw gc x y s (length s)
	  DrawImageString16 (Point x y) s ->
	    do let n = length s
	       cs <- marshallString16' s n
	       xDrawImageString16 d drw gc x y cs n
	  DrawString16 (Point x y) s ->
	    do let n = length s
	       cs <- marshallString16' s n
	       xDrawString16 d drw gc x y cs n
	  DrawRectangle (Rect (Point x1 y1) (Point x2 y2)) ->
            xDrawRectangle d drw gc x1 y1 x2 y2
	  FillRectangle (Rect (Point x1 y1) (Point x2 y2)) ->
            xFillRectangle d drw gc x1 y1 x2 y2
	  FillPolygon shape coordmode ps -> do
	      (xpoints,size) <- storePoints ps
	      xFillPolygon d drw gc xpoints size shape coordmode
	      freePtr xpoints
	  DrawLines coordmode ps -> do
	      (xpoints,size) <- storePoints ps
	      xDrawLines d drw gc xpoints size coordmode
	      freePtr xpoints
	  DrawArc (Rect (Point x y) (Point wi he)) a1 a2 ->
	      xDrawArc d drw gc (f x) (f y) (f wi) (f he) (f a1) (f a2)
            where f = fromIntegral
	  FillArc (Rect (Point x y) (Point wi he)) a1 a2 ->
	      xFillArc d drw gc (f x) (f y) (f wi) (f he) (f a1) (f a2)
            where f = fromIntegral
	  CopyArea src (Rect (Point srcx srcy) (Point wi he))
		       (Point dstx dsty) ->
	    xCopyArea d (getdr src) drw gc srcx srcy wi he dstx dsty
	  CopyPlane src (Rect (Point srcx srcy) (Point wi he)) 
			(Point dstx dsty) plane ->
            --print (gc,drw,cmd) >>
	    xCopyPlane d (getdr src)
		       drw gc (f srcx) (f srcy) (f wi) (f he)
                       (f dstx) (f dsty) (shiftL 1 plane)
            -- >>putStrLn "done"
            where f = fromIntegral
	  DrawPoint (Point x y) -> xDrawPoint d drw gc x y
	  CreatePutImage rect format pixels -> createPutImage drw gc rect format pixels
	  DrawImageStringPS (Point x y) s ->
	    xDrawImageString d drw gc x y (unpackPS s) (lengthPS s) -- !!
	  DrawStringPS (Point x y) s ->
	    xDrawString d drw gc x y (unpackPS s) (lengthPS s) -- !!
          _ -> hPutStr stderr (notImplemented cmd)

       createPutImage drw gc rect@(Rect (Point x y) (Point w h)) (ImageFormat format) pixels =
        do --hPutStrLn stderr $ "Entering createPutImage "++show rect
	   screen <- xDefaultScreen d
	   depth <- xDefaultDepth d screen
	   bpp <- default_bpp d depth
	   --hPutStrLn stderr ("bpp="++show bpp)
	   let byte_depth = (depth+7) `div` 8
	       bytes_pp = (bpp+7) `div` 8
	       bitmap_pad = 32
	       bytes_per_line = ((w*bytes_pp+3) `div` 4) * 4 -- assumes bitmap_pad == 32
	       nullCount = bytes_per_line - w*bytes_pp
	       size= w*h
	       pxlines = chopList (splitAt w) pixels
	   byteOrder <- xImageByteOrder d
#if 1
	   -- High level solution, not fast enough:
	   let pxlToBytes = if byteOrder==LSBFirst then lsb else msb
	       msb (Pixel p) = pad ++ reverse (lsb' byte_depth (fromIntegral p))
	       pad = replicate (bytes_pp-byte_depth) '\0'
	       lsb (Pixel p) = lsb' byte_depth (fromIntegral p) ++ pad
	       lsb' 0 _ = []
	       lsb' n p = toEnum (p `mod` 256) : lsb' (n-1) (p `div` 256)
	       linePad = replicate nullCount '\0'
	       byteLine pxls = concatMap pxlToBytes pxls ++ linePad
	       bytes = concatMap byteLine pxlines
	       --imgdata = psToByteArray (packString bytes)
           --hPutStrLn stderr "Checkpoint 1 in createPutImage"
           --hPutStrLn stderr $ "nullCount="++show nullCount
#else
	   -- Low level solution, faster, but still not fast enough:
	   imgdata <- stToIO $ newCharArray (1,bytes_per_line*h)
	   let convImage = convLines 0 pixels
	       convLines y pixels | y>=h = return ()
	                          | otherwise =
	         do pixels' <- convLine y pixels
		    convLines (y+1) pixels'
	       convLine y pixels = convPixels (y*bytes_per_line) 0 pixels
	       convPixels i x pixels | x>=w = return pixels
	       convPixels i x (Pixel p:pixels) =
		 do convPixel i p
		    convPixels (i+bytes_pp) (x+1) pixels
	       convPixel =
	         if byteOrder==(CCONST(LSBFirst)::Int)
		 then convPixelLSB
		 else convPixelMSB
	       convPixelLSB i p =
	           pixelBytes i p byte_depth
		   -- pad with zeros?
	         where
		   pixelBytes i p 0 = return ()
		   pixelBytes i p n =
		     do SINDEX(char,imgdata,i::Int,p::Int)
		        pixelBytes (i+1) (p `div` 256) (n-1)
	       convPixelMSB i p =
	           do let i' = i+bytes_pp-1
		      -- pad with zeros?
		      pixelBytes i' p bytes_pp
	         where
		   pixelBytes i p 0 = return ()
		   pixelBytes i p n =
		     do SINDEX(char,imgdata,i::Int,p::Int)
		        pixelBytes (i-1) (p `div` 256) (n-1)
		 
	    in convImage
#endif
	   dv <- xDefaultVisual d screen
           --hPutStrLn stderr "Checkpoint 2 in createPutImage"
           --if bytes==bytes then return () else undefined
           --hPutStrLn stderr "Checkpoint 3 in createPutImage"
	   cbytes <- marshallString' bytes (h*bytes_per_line)
	   image <- xCreateImage d dv depth format 0 cbytes w h bitmap_pad bytes_per_line
           --hPutStrLn stderr $ "Checkpoint 4 in createPutImage "++show image
	   xPutImage d drw gc image 0 0 x y w h
--	   ioCmd $ _casm_ ``((XImage *)(%0))->data=NULL;XDestroyImage((XImage *)%0);'' image
	   --_casm_ ``((XImage *)(%0))->data=NULL;'' image
           --hPutStrLn stderr "Checkpoint 5 in createPutImage"
	   setXImage_data image nullAddr
           --hPutStrLn stderr "Checkpoint 6 in createPutImage"
	   xDestroyImage image
           --hPutStrLn stderr "Checkpoint 7 in createPutImage"
	   freePtr cbytes
           --hPutStrLn stderr "Returning from createPutImage"
{-
       default_bpp :: Display -> Int -> IO Int
       default_bpp (Display display) depth =
         _casm_
	    ``{int i,cnt,bpp;
	       XPixmapFormatValues *ps=XListPixmapFormats(%0,&cnt);
	       bpp=%1; /* Hmm. Something is wrong if depth isn't found. */
	       for(i=0;i<cnt;i++)
	         if(ps[i].depth==%1) {
		   bpp=ps[i].bits_per_pixel;
		   break;
		 }
	       XFree(ps);
	       %r=bpp;}'' display depth
-}
foreign import ccall "asyncinput.h" default_bpp :: Display -> Int -> IO Int
foreign import ccall "asyncinput.h" setXImage_data :: CXImage -> Addr -> IO ()


getWindowAttributes = getValues newXSetWindowAttributes getWindowAttribute
 where
  getWindowAttribute swa wa =
   case wa of
    CWEventMask em    	 -> (SETWa(swa,event_mask,toC em),CWORD32(CWEventMask))
    CWBackingStore bs 	 -> (SETWa(swa,backing_store,toC bs),CWORD32(CWBackingStore))
    CWSaveUnder b     	 -> (SETWa(swa,save_under,toC b),CWORD32(CWSaveUnder))
    CWDontPropagate em   -> (SETWa(swa,do_not_propagate_mask,toC em),CWORD32(CWDontPropagate))
    CWOverrideRedirect b -> (SETWa(swa,override_redirect,toC b),CWORD32(CWOverrideRedirect))
    CWBackPixel p	 -> (SETWa(swa,background_pixel,toC p),CWORD32(CWBackPixel))
    CWCursor c		 -> (SETWaXID(swa,cursor,toXID c),CWORD32(CWCursor))
    CWBitGravity g 	 -> (SETWa(swa,bit_gravity,toC g),CWORD32(CWBitGravity))
    CWWinGravity g 	 -> (SETWa(swa,win_gravity,toC g),CWORD32(CWWinGravity))
    CWBackPixmap p       -> (SETWaXID(swa,background_pixmap,toXID p),CWORD32(CWBackPixmap))
    CWBorderPixmap p     -> (SETWaXID(swa,border_pixmap,toXID p),CWORD32(CWBorderPixmap))
    CWBorderPixel p      -> (SETWa(swa,border_pixel,toC p),CWORD32(CWBorderPixel) :: Bitmask)
    -- _ -> (return (),0) -- to skip unimplemented fields

getWindowChanges = getValues newXWindowChanges getWindowChange where
  getWindowChange s wc = case wc of
     CWX x 	      -> (SET(XWindowChanges,Int,s,x,x),CWORD32(CWX))
     CWY y 	      -> (SET(XWindowChanges,Int,s,y,y),CWORD32(CWY))
     CWWidth w        -> (SET(XWindowChanges,Int,s,width,w),CWORD32(CWWidth))
     CWHeight h       -> (SET(XWindowChanges,Int,s,height,h),CWORD32(CWHeight))
     CWBorderWidth w  -> (SET(XWindowChanges,Int,s,border_width,w),CWORD32(CWBorderWidth))
     CWStackMode sm   -> (SET(XWindowChanges,Int,s,stack_mode,toC sm),CWORD32(CWStackMode) :: Bitmask) 

{- deriving toEnum & fromEnum appears broken (not anymore /TH 2000-11-28) -}

instance ToC CoordMode where toC = getEnum CoordModeOrigin
instance ToC Shape where toC = getEnum Complex
instance ToC BackingStore where toC = getEnum NotUseful
instance ToC Gravity where toC = getEnum ForgetGravity
instance ToC StackMode where toC = getEnum StackAbove
instance ToC ShapeKind where toC = getEnum ShapeBounding
instance ToC ShapeOperation where toC = getEnum ShapeSet
instance ToC Ordering' where toC = getEnum Unsorted

instance ToC Button where toC AnyButton  = CCONST(AnyButton)
			  toC (Button i) = i

getEvent w e = do
    xe <- newXEvent
    SET(XAnyEvent,Window,xe,window,w::Window)
    case e of
      SelectionNotify time (Selection sel target props) -> do
        SET(XSelectionEvent,Int,xe,type,CCONST(SelectionNotify)::Int)
        SET(XSelectionEvent,Atom,xe,selection, sel)
        SET(XSelectionEvent,Atom,xe,target, target)
        SET(XSelectionEvent,Atom,xe,property, props)
        SET(XSelectionEvent,Time,xe,time,time)
    return xe


storePoints ps = getArray newXPointArray
	          (\xpoints (i,Point x y) -> do SETI(XPoint,Int,xpoints,i,x,x)
					        SETI(XPoint,Int,xpoints,i,y,y)) ps

storeRectangles rs =
  getArray newXRectangleArray
           (\rsa (i,Rect (Point x y) (Point w h)) -> do
		     SETI(XRectangle,Int,rsa,i,x,x)
		     SETI(XRectangle,Int,rsa,i,y,y)
		     SETI(XRectangle,Int,rsa,i,width,w)
		     SETI(XRectangle,Int,rsa,i,height,h)) rs
