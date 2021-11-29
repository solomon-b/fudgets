{-# LANGUAGE CPP #-}
{- Obsolete OPTIONS -optc-I/usr/X11R6/include -#include <X11/Xlib.h> -#include <X11/Xutil.h> -#include <X11/extensions/shape.h> -#include <X11/extensions/Xdbe.h> -fvia-C -}
module Xlib where
import Marshall
import MyForeign
import CSizes
import Xtypes
import Visual
import Font(FontDirection)
import CString16(CString16)

import DrawTypes

import Control.Monad(zipWithM_)
--import Ap
import Data.Word(Word,Word32)
--import CCall
default(Int)

#include "structs.h"

type Unsigned = Int -- hmm
type Unsigned32 = Int32 -- hmm
type Status = Int
type Screen = Int
type Bitmask = Word32
type XlibKeySym = XID -- KeySym in Xlib, defined as String in Fudgets
type ClipOrdering = Ordering'
type CPixelArray = CLong
type CStringArray = CString
type CXFontStructArray = CXFontStruct
type CDisplay = Addr
type CGCId = Addr
newtype Region = Region Addr

#define FI(f) foreign import ccall unsafe "f" prim/**/f
#define PXlib(f,p,h) FI(X/**/f) :: p ; x/**/f :: h ; x/**/f = call primX/**/f
#define Xlib(f,h) PXlib(f,h,h)

#define PReq0(f,pr,r) PXlib(f,CDisplay -> IO pr, Display -> IO r)
#define Req0(f,r) PReq0(f,r,r)
#define Req(f,t,r) PReq(f,t,r,t,r)
#define PReq(f,p,pr,t,r) PXlib(f,CDisplay -> p -> IO pr, Display-> t ->IO r)
#define WindowReq(f,t,r) Req(f,Window->t,r)
#define PWindowReq(f,pt,pr,t,r) PReq(f,Window->pt,pr,Window->t,r)
#define WindowReqP(f,t,pr,r) PReq(f,Window->t,pr,Window->t,r)
#define DrawReq(f,t,r) Req(f,DrawableId->t,r)
#define DrawReqP(f,t,pr,r) PReq(f,DrawableId->t,pr,DrawableId->t,r)

#define Cmd0(f) PXlib(f,CDisplay -> IO (), Display-> IO ())
#define Cmd(f,t) Req(f,t,())
#define PCmd(f,p,t) PXlib(f,CDisplay -> p -> IO (), Display-> t ->IO ())
#define WindowCmd0(f) Cmd(f,Window)
#define WindowCmd(f,t) Cmd(f,Window->t)
#define PWindowCmd(f,p,t) PCmd(f,Window->p,Window->t)
#define DrawCmd(f,t) PCmd(f,DrawableId->CGCId->t,DrawableId->GCId->t)
#define DrawPCmd(f,p,t) PCmd(f,DrawableId->CGCId->p,DrawableId->GCId->t)

--- Calls ---------------------------------------------------------------------
PXlib(OpenDisplay,  CString -> IO CDisplay, String -> IO Display)
Cmd0(CloseDisplay)
Req0(ConnectionNumber,Int32)
Cmd0(Flush)
Cmd(NextEvent,CXEvent)
WindowReq(CheckWindowEvent,Int->CXEvent,Bool)
Req0(Pending,Int)
Cmd(FreePixmap,PixmapId)
Cmd(Synchronize,Bool) -- result is ignored.
Cmd(Sync,Bool)
Cmd(Bell,Int)

FI(XInitThreads) :: IO Int
xInitThreads = fmap (/=0) primXInitThreads
Cmd0(LockDisplay)
Cmd0(UnlockDisplay)

FI(XFree) :: Addr -> IO ()
xFree p = primXFree (addrOf p)

Req0(DefaultScreen,Screen)
Req0(DefaultRootWindow,Window)
PReq0(ImageByteOrder,Int,ByteOrder)
Req(DefaultDepth,Screen,Int)
Req(BlackPixel,Int,Unsigned)
Req(WhitePixel,Int,Unsigned)
Req(DefaultColormap,Int,ColormapId)
Req(DefaultVisual,Screen,CVisual)

PReq(LoadFont,CString,FontId,String,FontId)
Req(QueryFont,FontId,CXFontStruct)
PReq(LoadQueryFont,CString,CXFontStruct,String,CXFontStruct)
PReq(ListFonts,CString->Int->CLong,CStringArray,String->Int->CLong,CStringArray)
Xlib(FreeFontNames,CStringArray->IO())
PReq(ListFontsWithInfo,CString->Int->CInt32->CCXFontStruct,CStringArray,String->Int->CInt32->CCXFontStruct,CStringArray)
Xlib(FreeFontInfo,CStringArray->CXFontStructArray->Int32->IO ())
Cmd(QueryTextExtents16,FontId->CString16->Int->CLong->CLong->CLong->CXCharStruct)

Req(CreateFontCursor,Int,CursorId)
Req(CreatePixmap,DrawableId->Unsigned->Unsigned->Unsigned,PixmapId)
PReq(InternAtom,CString->Bool,Atom,String->Bool,Atom)
Req(GetAtomName,Word32,CString)
Req(AllocColor,ColormapId->CXColor,Status)
PReq(AllocNamedColor,ColormapId->CString->CXColor->CXColor,Status,ColormapId->String->CXColor->CXColor,Status)
Cmd(QueryColor,ColormapId->CXColor)
Cmd(FreeColors,ColormapId->CPixelArray->Int->Pixel)
{- -- This creates a dangling reference to the image data!
PReq(CreateImage,CVisual->Int->Int->Int->CString->Int->Int->Int->Int,CXImage,
                 CVisual->Int->Int->Int->String->Int->Int->Int->Int,CXImage)
-}
Req(CreateImage,CVisual->Int->Int->Int->CString->Int->Int->Int->Int,CXImage)

DrawCmd(PutImage,CXImage->Int->Int->Int->Int->Int->Int)

-- Xlib(DestroyImage,CXImage->IO ()) -- XDestroyImage is a macro in X11/Xutil.h
foreign import ccall "asyncinput.h" xDestroyImage :: CXImage -> IO ()


WindowCmd0(DestroyWindow)
WindowCmd0(MapRaised)
WindowCmd0(LowerWindow)
WindowCmd0(UnmapWindow)
WindowCmd0(ClearWindow)
WindowCmd(ClearArea,Int->Int->Int->Int->Bool)
WindowReq(CreateSimpleWindow,Int->Int->Unsigned->Unsigned->Unsigned->Unsigned->Unsigned,Window)
PWindowCmd(StoreName,CString,String)
WindowCmd(SetClassHint,CXClassHint)
WindowCmd(ConfigureWindow,Bitmask->CXWindowChanges)
WindowCmd(ChangeWindowAttributes,Bitmask->CXSetWindowAttributes)
WindowCmd(ReparentWindow,Window->Int->Int)
PWindowCmd(SetWMProtocols,CAtomArray->Int,[Atom]->Int)
WindowCmd(SetNormalHints,CXSizeHints)
WindowCmd(SetWMHints,CXWMHints)
WindowReq(SendEvent,Bool->Bitmask->CXEvent,Status)
PWindowCmd(ChangeProperty,Atom->Atom->Int->PropertyMode->CString->Int,Atom->Atom->Int->PropertyMode->String->Int)
WindowReq(GetWindowProperty,Atom->Int->Int->Bool->Atom->CAtom->CLong->CLong->CLong->CCString,Int)
WindowReq(QueryPointer,CLong->CLong->CLong->CLong->CLong->CLong->CLong,Bool)
WindowReq(TranslateCoordinates,Window->Int->Int->CInt32->CInt32->CLong,Bool)
PWindowCmd(ShapeCombineMask,Int->Int->Int->PixmapId->Int,ShapeKind->Int->Int->PixmapId->ShapeOperation)
PWindowCmd(ShapeCombineRectangles,Int->Int->Int->CXRectangleArray->Int->Int->Int,ShapeKind->Int->Int->CXRectangleArray->Int->ShapeOperation->ClipOrdering)
PWindowCmd(ShapeCombineShape,Int->Int->Int->PixmapId->Int->Int,ShapeKind->Int->Int->PixmapId->ShapeKind->ShapeOperation)

Cmd(GrabButton,Int->Int->Window->Bool->Unsigned->Int->Int->Window->CursorId)
Cmd(UngrabButton,Int->Int->Window)
WindowReqP(GrabPointer,Bool->Unsigned->Int->Int->Window->CursorId->Time,Int,GrabPointerResult)
Cmd(UngrabPointer,Time)
Cmd(SetSelectionOwner,Atom->Window->Time)
Cmd(ConvertSelection,Atom->Atom->Atom->Window->Time)

DrawCmd(DrawPoint,Int->Int)
DrawCmd(DrawLine,Int->Int->Int->Int)
DrawPCmd(DrawLines,CXPointArray->Int->Int,CXPointArray->Int->CoordMode)
DrawPCmd(DrawImageString,Int->Int->CString->Int,Int->Int->String->Int)
DrawPCmd(DrawString,Int->Int->CString->Int,Int->Int->String->Int)
DrawPCmd(DrawImageString16,Int->Int->CString16->Int,Int->Int->CString16->Int)
DrawPCmd(DrawString16,Int->Int->CString16->Int,Int->Int->CString16->Int)
DrawCmd(DrawRectangle,Int->Int->Int->Int)
DrawCmd(FillRectangle,Int->Int->Int->Int)
DrawCmd(DrawArc,Int32->Int32->Unsigned32->Unsigned32->Int32->Int32)
DrawCmd(FillArc,Int32->Int32->Unsigned32->Unsigned32->Int32->Int32)
DrawPCmd(FillPolygon,CXPointArray->Int->Int->Int,CXPointArray->Int->Shape->CoordMode)
PCmd(CopyArea,DrawableId->DrawableId->CGCId->Int->Int->Unsigned->Unsigned->Int->Int,DrawableId->DrawableId->GCId->Int->Int->Unsigned->Unsigned->Int->Int)
PCmd(CopyPlane,DrawableId->DrawableId->CGCId->Int32->Int32->Unsigned32->Unsigned32->Int32->Int32->Word,DrawableId->DrawableId->GCId->Int32->Int32->Unsigned32->Unsigned32->Int32->Int32->Word)

#define GCCmd(f,t) PCmd(f,CGCId->t,GCId->t)

DrawReqP(CreateGC,Bitmask->CXGCValues,CGCId,GCId)
PCmd(CopyGC,CGCId->Bitmask->CGCId,GCId->Bitmask->GCId)
GCCmd(ChangeGC,Bitmask->CXGCValues)
PCmd(FreeGC,CGCId,GCId)

GCCmd(SetForeground,Unsigned)
GCCmd(SetBackground,Unsigned)
GCCmd(SetPlaneMask,Bitmask)
PCmd(SetFunction,CGCId->Int,GCId->GCFunction)
PCmd(SetState,CGCId->Unsigned->Unsigned->Int->Unsigned,GCId->Unsigned->Unsigned->GCFunction->Unsigned)

PXlib(KeysymToString,XlibKeySym->IO CString,XlibKeySym->IO (Maybe String))

Req(ReadBitmapFile,DrawableId->CString->CInt32->CInt32->CXID->CInt32->CInt32,Int)
Req(CreateBitmapFromData,DrawableId->CString->Int32->Int32,PixmapId)

--- Regions -------------------------------------------------------------------
Xlib(CreateRegion,IO Region)
PCmd(SetRegion,CGCId->Region,GCId->Region)
Xlib(DestroyRegion,Region->IO ())
Xlib(UnionRectWithRegion,CXRectangle->Region->Region->IO ())
IDAR(Region)

--- Double buffer extension ---------------------------------------------------
Req(dbeQueryExtension,CLong->CLong,Int)
PWindowReq(dbeAllocateBackBufferName,Int,DbeBackBufferId,SwapAction,DbeBackBufferId)
Req(dbeSwapBuffers,CXdbeSwapInfoArray->Int,Status)

H_ARRAY(XdbeSwapInfo)
ENUMAR(SwapAction)
IDAR(DbeBackBufferId)
--- Types ---------------------------------------------------------------------

IORC(CDisplay,Display,fmap (Display . a2i))
instance PrimArg Display CDisplay c where marshall c (Display d) = c (i2a d)

IORC(CGCId,GCId,fmap (GCId . a2i))

instance PrimArg GCId CGCId c where marshall c (GCId d) = c (i2a d)


-- Instances for resource identifiers and simple values
IDAR(Atom)
IDAR(ColormapId)
IDAR(CursorId)
IDAR(DrawableId)
IDAR(FontId)
IDAR(Pixel)
IDAR(PixmapId)
IDAR(PropertyMode)
IDAR(VisualID)
IDAR(Window)
IDAR0(Word32)
IDAR0(Word)
IDAR0(XID)
ENUMAR(ByteOrder)
ENUMAR(CoordMode)
ENUMAR(DisplayClass)
ENUMAR(FontDirection)
ENUMAR(GrabPointerResult)
ENUMAR(GCFunction)
ENUMAR(GCLineStyle)
ENUMAR(GCCapStyle)
ENUMAR(GCJoinStyle)
ENUMAR(GCSubwindowMode)
ENUMAR(GCFillStyle)
ENUMAR(ClipOrdering)
ENUMAR(Shape)
ENUMAR(ShapeKind)
ENUMAR(ShapeOperation)

IDAR0(CXID)
ISTORE(XID)

instance CVar CXID XID

--- Structured types ----------------------------------------------------------
H_STRUCTTYPE(XColor)
H_STRUCTTYPE(XClassHint)
H_STRUCTTYPE(XGCValues)
H_STRUCTTYPE(XSetWindowAttributes)
H_STRUCTTYPE(XSizeHints)
H_STRUCTTYPE(XWMHints)
H_STRUCTTYPE(XWindowChanges)

H_ARRAY(Atom)
ISTORE(Atom)
instance CVar CAtom Atom
instance PrimArg [Atom] CAtom (Int->IO a) where
  marshall pf as n =
    do aa <- newArray n
       zipWithM_ (pokeElemOff (addrOf aa)) [0..(n-1)] [a|Atom a<-as]
       r<-pf aa n
       freePtr aa
       return r

H_STRUCTTYPE(XEvent)
type CXAnyEvent = CXEvent
type CXKeyEvent = CXEvent
type CXButtonEvent = CXEvent
type CXMotionEvent = CXEvent
type CXCrossingEvent = CXEvent
type CXFocusChangeEvent = CXEvent
type CXExposeEvent = CXEvent
type CXGraphicsExposeEvent = CXEvent
type CXNoExposeEvent = CXEvent
type CXVisibilityEvent = CXEvent
type CXCreateWindowEvent = CXEvent
type CXDestroyWindowEvent = CXEvent
type CXUnmapEvent = CXEvent
type CXMapEvent = CXEvent
type CXMapRequestEvent = CXEvent
type CXReparentEvent = CXEvent
type CXConfigureEvent = CXEvent
type CXGravityEvent = CXEvent
type CXResizeRequestEvent = CXEvent
type CXConfigureRequestEvent = CXEvent
type CXCirculateEvent = CXEvent
type CXClientMessageEvent = CXEvent
type CXSelectionClearEvent = CXEvent
type CXSelectionRequestEvent = CXEvent
type CXSelectionEvent = CXEvent

H_ARRAY(XPoint)
H_ARRAY(XRectangle)
C_STRUCTTYPE(XFontStruct);IDAR0(CXFontStruct)

--C_STRUCTTYPE(XCharStruct);IDAR0(CXCharStruct)
NEWTYPE(HT(XCharStruct));IPTR(XCharStruct);ISTORE(HT(XCharStruct));
INSTCCALL(HT(XCharStruct));IDAR0(CXCharStruct)

C_STRUCTTYPE(Visual);IDAR0(CVisual)
C_STRUCTTYPE(XImage);IDAR0(CXImage)
INSTCCALL(CString)

NEWTYPE(HT(XFontProp));IPTR(XFontProp);ISTORE(HT(XFontProp));
INSTCCALL(HT(XFontProp));IDAR0(CXFontProp)

C_STRUCTTYPE(CXFontStruct);IDAR0(CCXFontStruct)
newCXFontStruct = CCXFontStruct <$> mallocElem nullAddr
instance CVar CCXFontStruct CXFontStruct

C_STRUCTTYPE(CString);IDAR0(CCString)
newCString = CCString <$> mallocElem nullAddr
instance CVar CCString CString
--- Constants -----------------------------------------------------------------
anyModifier = 2^(15 :: Int)
grabModeAsync = 1 :: Int
--- Auxiliary functions/types -------------------------------------------------
call f = unmarshall f
uio f = fmap f

a2i :: Addr -> Int
a2i a = minusAddr a nullAddr

{-# NOINLINE i2a #-} -- Workaround a GHC bug on 64-bit systems /TH 2016-12-29
i2a :: Int -> Addr
i2a = plusAddr nullAddr

newtype DrawableId = DrawableId XID deriving Show
getdrawable _ (Pixmap (PixmapId i)) = DrawableId i
getdrawable (WindowId w) MyWindow = DrawableId w
getdrawable _ (DbeBackBuffer (DbeBackBufferId b)) = DrawableId b

dcmap display cmap =
    if cmap == defaultColormap
    then xDefaultScreen display >>= xDefaultColormap display
    else return cmap

data ByteOrder = LSBFirst | MSBFirst deriving (Eq,Enum)

--withLockedDisplay display cmd = cmd
{-
withLockedDisplay display cmd =
  do xLockDisplay display
     result <- cmd
     xUnlockDisplay display
     return result
-}
