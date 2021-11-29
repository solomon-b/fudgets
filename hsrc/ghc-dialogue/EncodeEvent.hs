{-# LANGUAGE CPP #-}
{- Obsolete OPTIONS -optc-I/usr/X11R6/include -#include <X11/Xlib.h> -#include <X11/Xutil.h> -fvia-C -}
module EncodeEvent(getNextEvent,motionCompress) where

import Event
import Xtypes
--import ResourceIds

import XCallTypes
import StructFuns
import Xlib
import Marshall
import MyForeign
import qualified Foreign as F

import FudUTF8(decodeUTF8)
import Data.Maybe(fromMaybe)
--import Ap

--import GlaExts hiding(Word) --(primIOToIO,CCallable,CReturnable,Addr)
--import MutableArray(MutableByteArray#)
--import PrelGHC
--import PrelBase
--import GhcWord
--import CString(unpackCStringIO)
--import Command
--import Font

#include "newstructfuns.h"

getNextEvent d = do
   ev <- newXEvent
   xNextEvent d ev
   window <- GET(XAnyEvent,WindowId,ev,window)
   fev <- encodeEvent ev
   return (window,fev) -- no freePtr?!

checkWindowEvent d w evmask = do
   ev <- newXEvent
   found <- xCheckWindowEvent d w (toC (evmask::[EventMask])) ev
   if found
     then Just <$> encodeEvent ev
     else freePtr ev >> return Nothing

motionCompress d e@(w,fev@MotionNotify{}) =
  do me <- checkWindowEvent d w [PointerMotionMask]
     case me of
       Nothing -> return e
       Just fev' -> motionCompress d (w,fev')
motionCompress _ e = return e


encodeEvent ev = do
   evno <- GET(XAnyEvent,Int,ev,type)
   case evno :: Int of

{- ghc doesn't handle literal (``KeyPress'') patterns yet -}

#define X_KeyPress		2
#define X_KeyRelease		3
#define X_ButtonPress		4
#define X_ButtonRelease		5
#define X_MotionNotify		6
#define X_EnterNotify		7
#define X_LeaveNotify		8
#define X_FocusIn		9
#define X_FocusOut		10
#define X_KeymapNotify		11
#define X_Expose		12
#define X_GraphicsExpose	13
#define X_NoExpose		14
#define X_VisibilityNotify	15
#define X_CreateNotify		16
#define X_DestroyNotify		17
#define X_UnmapNotify		18
#define X_MapNotify		19
#define X_MapRequest		20
#define X_ReparentNotify	21
#define X_ConfigureNotify	22
#define X_ConfigureRequest	23
#define X_GravityNotify		24
#define X_ResizeRequest		25
#define X_CirculateNotify	26
#define X_CirculateRequest	27
#define X_PropertyNotify	28
#define X_SelectionClear	29
#define X_SelectionRequest	30
#define X_SelectionNotify	31
#define X_ColormapNotify	32
#define X_ClientMessage		33
#define X_MappingNotify		34
     X_KeyPress      	-> putKeyEvent ev Pressed
     X_KeyRelease    	-> putKeyEvent ev Released
     X_ButtonPress   	-> putButtonEvent ev Pressed
     X_ButtonRelease 	-> putButtonEvent ev Released
     X_MotionNotify  	-> putMotionEvent ev
     X_EnterNotify   	-> putCrossingEvent ev True
     X_LeaveNotify   	-> putCrossingEvent ev False
     X_FocusIn       	-> putFocusChangeEvent ev True
     X_FocusOut      	-> putFocusChangeEvent ev False
     X_KeymapNotify  	-> return KeymapNotify
     X_Expose	       	-> putExposeEvent ev
     X_GraphicsExpose	-> putGraphicsExposeEvent ev
     X_NoExpose		-> return NoExpose
     X_VisibilityNotify	-> putVisibilityEvent ev
     X_CreateNotify	-> putStructEvent ev CreateNotify
     X_DestroyNotify	-> putStructEvent ev DestroyNotify
     X_UnmapNotify	-> putStructEvent ev UnmapNotify
     X_MapNotify	-> putStructEvent ev MapNotify
     X_MapRequest	-> putStructEvent ev MapRequest
     X_ReparentNotify	-> return ReparentNotify
     X_ConfigureNotify	-> putConfigureEvent ev
     X_ConfigureRequest	-> return ConfigureRequest
     X_GravityNotify	-> return GravityNotify
     X_ResizeRequest	-> putResizeRequestEvent ev
     X_CirculateNotify	-> return CirculateNotify
     X_CirculateRequest	-> return CirculateRequest
     X_PropertyNotify	-> return PropertyNotify
     X_SelectionClear	-> putSelectionClearEvent ev
     X_SelectionRequest	-> putSelectionRequestEvent ev
     X_SelectionNotify	-> putSelectionNotifyEvent ev
     X_ColormapNotify	-> return ColormapNotify
     X_ClientMessage	-> putClientMessageEvent ev
     X_MappingNotify	-> return MappingNotify

mkClientData ev = do
  format <- GET(XClientMessageEvent,Int,ev,format)
  d <- AGET(XClientMessageEvent,Addr,ev,data)
  case format::Int of
    32 -> Long <$> mapM (CINDEX(long) (d::Addr)) [0..(4::Int)]
    16 -> Short <$> mapM (CINDEX(short) d) [0..(9::Int)]
    8  -> Byte <$> mapM (CINDEX(char) d) [0..(19::Int)]

putKeyEvent ev p = do
  (key,l) <- xLookupString ev 100
  KeyEvent
   <$> GET(XKeyEvent,Time,ev,time)
   <*> mkPoint GET(XKeyEvent,Int,ev,x) GET(XKeyEvent,Int,ev,y)
   <*> mkPoint GET(XKeyEvent,Int,ev,x_root) GET(XKeyEvent,Int,ev,y_root)
   <*> fmap fromC GET(XKeyEvent,Int,ev,state)
   <*> return p
   <*> fmap KeyCode GET(XKeyEvent,Int,ev,keycode)
   <*> return key
   <*> return (decodeUTF8 l)
             -- !! Assume that XLookupString returns a UTF-8 string
             -- https://gitlab.freedesktop.org/xorg/lib/libx11/-/issues/39

putButtonEvent ev p =
   ButtonEvent
   <$> GET(XButtonEvent,Time,ev,time)
   <*> mkPoint GET(XButtonEvent,Int,ev,x) GET(XButtonEvent,Int,ev,y)
   <*> mkPoint GET(XButtonEvent,Int,ev,x_root) GET(XButtonEvent,Int,ev,y_root)
   <*> fmap fromC GET(XButtonEvent,Int,ev,state)
   <*> return p
   <*> fmap Button GET(XButtonEvent,Int,ev,button)

putMotionEvent ev =
   MotionNotify
   <$> GET(XMotionEvent,Time,ev,time)
   <*> mkPoint GET(XMotionEvent,Int,ev,x) GET(XMotionEvent,Int,ev,y)
   <*> mkPoint GET(XMotionEvent,Int,ev,x_root) GET(XMotionEvent,Int,ev,y_root)
   <*> fmap fromC GET(XMotionEvent,Int,ev,state)

putCrossingEvent ev c =
   (if c then EnterNotify else LeaveNotify)
   <$> GET(XCrossingEvent,Time,ev,time)
   <*> mkPoint GET(XCrossingEvent,Int,ev,x) GET(XCrossingEvent,Int,ev,y)
   <*> mkPoint GET(XCrossingEvent,Int,ev,x_root) GET(XCrossingEvent,Int,ev,y_root)
   <*> fmap fromC GET(XCrossingEvent,Int,ev,detail)
   <*> fmap fromC GET(XCrossingEvent,Int,ev,mode)
   <*> fmap fromC GET(XCrossingEvent,Int,ev,focus)


putFocusChangeEvent ev c =
   (if c then FocusIn else FocusOut)
   <$> fmap fromC GET(XFocusChangeEvent,Int,ev,detail)
   <*> fmap fromC GET(XFocusChangeEvent,Int,ev,mode)

putExposeEvent ev =
   Expose
   <$> mkRect GET(XExposeEvent,Int,ev,x) GET(XExposeEvent,Int,ev,y)
	       GET(XExposeEvent,Int,ev,width) GET(XExposeEvent,Int,ev,height) 
   <*> GET(XExposeEvent,Int,ev,count)

putGraphicsExposeEvent ev =
   GraphicsExpose 
   <$> mkRect GET(XGraphicsExposeEvent,Int,ev,x) GET(XGraphicsExposeEvent,Int,ev,y)
	       GET(XGraphicsExposeEvent,Int,ev,width) GET(XGraphicsExposeEvent,Int,ev,height) 
   <*> GET(XGraphicsExposeEvent,Int,ev,count)
   <*> GET(XGraphicsExposeEvent,Int,ev,major_code)
   <*> GET(XGraphicsExposeEvent,Int,ev,minor_code)

putVisibilityEvent ev =
  VisibilityNotify . fromC <$> GET(XVisibilityEvent,Int,ev,state)

putStructEvent ev c = c <$> GET(XMapEvent,WindowId,ev,window)

putConfigureEvent ev =
   ConfigureNotify
   <$> mkRect GET(XConfigureEvent,Int,ev,x) GET(XConfigureEvent,Int,ev,y)
               GET(XConfigureEvent,Int,ev,width) GET(XConfigureEvent,Int,ev,height)
   <*> GET(XConfigureEvent,Int,ev,border_width)

putResizeRequestEvent ev =
   ResizeRequest 
  <$> mkPoint GET(XResizeRequestEvent,Int,ev,width)
             GET(XResizeRequestEvent,Int,ev,height)

putSelectionClearEvent ev = SelectionClear
 <$> GET(XSelectionClearEvent,Atom,ev,selection)

putSelectionRequestEvent ev =
  SelectionRequest
  <$> GET(XSelectionRequestEvent,Time,ev,time)
  <*> GET(XSelectionRequestEvent,WindowId,ev,requestor)
  <*> (Selection <$> GET(XSelectionRequestEvent,Atom,ev,selection)
		<*> GET(XSelectionRequestEvent,Atom,ev,target)
		<*> GET(XSelectionRequestEvent,Atom,ev,property))

putSelectionNotifyEvent ev =
  SelectionNotify
  <$> GET(XSelectionEvent,Time,ev,time)
  <*> (Selection <$> GET(XSelectionEvent,Atom,ev,selection)
		<*> GET(XSelectionEvent,Atom,ev,target)
		<*> GET(XSelectionEvent,Atom,ev,property))

putClientMessageEvent ev =
  ClientMessage
  <$> GET(XClientMessageEvent,Atom,ev,message_type)
  <*> mkClientData ev

foreign import ccall "X11/Xlib.h XLookupString"
  cXLookupString :: CXKeyEvent -> Addr -> Int -> F.Ptr XlibKeySym -> Addr -> IO Int

xLookupString :: CXKeyEvent -> Int -> IO (KeySym,String)
xLookupString xev len =
    alloca len $ \ arr ->
    F.alloca $ \ keysyma ->
    do len' <- {- _casm_ ``%r=(int)XLookupString((XKeyEvent *)%0,
					  (char *)%1,
					  (int)%2,
					  (KeySym *)%3,
					  NULL);'' xev arr len keysyma -}
           cXLookupString xev arr len keysyma nullAddr
       keysym <- F.peek keysyma
       key <- xKeysymToString keysym
       let key' = fromMaybe "(undefined)" key
       str <- unmarshallString' (CString arr) len'
       return (key',str)

instance FromC Detail where fromC = toEnum' NotifyAncestor
instance FromC Mode where fromC = toEnum' NotifyNormal
instance FromC Visibility where fromC = toEnum' VisibilityUnobscured


instance F.Storable XID where
  sizeOf (XID x) = F.sizeOf x
  alignment (XID x) = F.alignment x

  peek a = fmap XID (F.peek (F.castPtr a))
  poke a (XID x) = F.poke (F.castPtr a) x
