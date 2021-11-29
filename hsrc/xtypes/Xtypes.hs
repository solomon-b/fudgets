module Xtypes(module Xtypes,module EventMask,module AuxTypes,module ResourceIds,module Image) where
--import Geometry(Line, Point, Rect)
import EventMask
import AuxTypes
import ResourceIds
import Image
import Data.Ix
import HbcWord

-- #ifdef __NHC__
-- Bug in export of newtype
-- #define newtype data
-- #endif

newtype Display = Display Int deriving (Eq, Ord, Show, Read)
type XDisplay = Display

noDisplay = Display (-1)

type KeyLookup = String
type Width = Int

newtype Pixel = Pixel Word deriving (Eq, Ord, Show, Read)

type PlaneMask = Pixel

pixel0 = Pixel 0
pixel1 = Pixel 1
--black = Pixel 0 -- not always true !!
--white = Pixel 1 -- not always true !!

data RGB = RGB Int Int Int deriving ( Eq, Ord, Show, Read, Ix )
data Color = Color { colorPixel::Pixel, colorRGB::RGB }
             deriving (Eq, Ord, Show, Read)

maxRGB :: Int -- for hugs
maxRGB = 65535
grayRGB x = RGB x x x
whiteRGB = grayRGB maxRGB
blackRGB = grayRGB 0

data Selection = Selection Atom Atom Atom  deriving (Eq, Ord, Show, Read)


newtype PropertyMode = PropertyMode Int deriving (Eq, Ord, Show, Read)

propModeReplace = PropertyMode 0
propModePrepend = PropertyMode 1
propModeAppend  = PropertyMode 2

--data EventMask -- moved to EventMask.hs

data BackingStore
  = NotUseful | WhenMapped | Always 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GrabPointerResult
  = GrabSuccess
  | AlreadyGrabbed
  | GrabInvalidTime
  | GrabNotViewable
  | GrabFrozen 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Same order as in X.h
data GCFunction
  = GXclear
  | GXand
  | GXandReverse
  | GXcopy
  | GXandInverted
  | GXnoop
  | GXxor
  | GXor
  | GXnor
  | GXequiv
  | GXinvert
  | GXorReverse
  | GXCopyInverted
  | GXorInverted
  | GXnand
  | GXset 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- Same order as in X.h
data GCLineStyle
  = LineSolid | LineDoubleDash | LineOnOffDash 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GCCapStyle
  = CapNotLast | CapButt | CapRound | CapProjecting 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GCJoinStyle
  = JoinMiter | JoinRound | JoinBevel
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
  
data GCSubwindowMode
  = ClipByChildren | IncludeInferiors 
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GCFillStyle
  = FillSolid | FillTiled | FillStippled | FillOpaqueStippled
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data GCAttributes a b
  = GCFunction GCFunction
  | GCForeground a
  | GCBackground a
  | GCLineWidth Width
  | GCLineStyle GCLineStyle
  | GCFont b
  | GCCapStyle GCCapStyle
  | GCSubwindowMode GCSubwindowMode
  | GCGraphicsExposures Bool
  | GCFillStyle GCFillStyle
  | GCTile PixmapId
  | GCStipple PixmapId
  | GCJoinStyle GCJoinStyle
  deriving (Eq, Ord, Show, Read)

type GCAttributeList = [GCAttributes Pixel FontId]

--invertGCattrs = invertColorGCattrs white black

invertColorGCattrs bgcol fgcol =
    [GCFunction GXxor, GCForeground (invcol bgcol fgcol)]

invcol (Pixel bg) (Pixel fg) = Pixel (bitXor bg fg)

data WindowAttributes
  = CWEventMask [EventMask]
  | CWBackingStore BackingStore
  | CWSaveUnder Bool
  | CWDontPropagate [EventMask]
  | CWOverrideRedirect Bool
  | CWBackPixel Pixel
  | CWCursor CursorId
  | CWBitGravity Gravity
  | CWWinGravity Gravity
  | CWBackPixmap PixmapId
  | CWBorderPixmap PixmapId
  | CWBorderPixel Pixel
  deriving (Eq, Ord, Show, Read)

data WindowChanges
  = CWX Int
  | CWY Int
  | CWWidth Int
  | CWHeight Int
  | CWBorderWidth Int
  | CWStackMode StackMode 
  deriving (Eq, Ord, Show, Read)

data StackMode
  = StackAbove | StackBelow | TopIf | BottomIf | Opposite 
  deriving (Eq, Ord, Read, Show, Bounded, Enum)


-- DBE (double buffering extension):
data SwapAction
  = DbeUndefined | DbeBackground | DbeUntouched | DbeCopied
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
