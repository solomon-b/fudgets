module Command(module Command,DrawCommand,Drawable(..)) where
import Event(XEvent)
import Direction() -- for hbc
import Path(Path(..))
import Geometry
--import LayoutRequest(LayoutMessage(..))
import Xtypes
import DrawTypes
--import DialogueIO(Request)
import ShowFun()

-- Same order as in .../lml/src/lib/xlib/xlib.h
data XCommand -- X commands
  = CloseDisplay Display
  | DestroyWindow
  | MapRaised
  | LowerWindow
  | UnmapWindow
  | Draw Drawable GCId DrawCommand
  | DrawMany Drawable [(GCId,[DrawCommand])] -- use instead of XDoCommands
  | ClearArea Rect Bool
  | ClearWindow
  | ChangeGC GCId GCAttributeList
  | FreeGC GCId
  | ChangeWindowAttributes [WindowAttributes]
  | ConfigureWindow [WindowChanges]
  | StoreName String
  | SetNormalHints Point
  | SetWMHints Bool -- input
  | UngrabPointer
  | GrabButton Bool Button ModState [EventMask]
  | UngrabButton Button ModState
  | Flush
  | FreePixmap PixmapId
  | ShapeCombineMask ShapeKind Point PixmapId ShapeOperation
  | ShapeCombineRectangles ShapeKind Point [Rect] ShapeOperation Ordering'
  | ShapeCombineShape ShapeKind Point PixmapId ShapeKind ShapeOperation
  | RmDestroyDatabase RmDatabase
  | RmCombineDatabase RmDatabase RmDatabase Bool
  | RmPutLineResource RmDatabase String
  | SetWMProtocols [Atom]
  | SendEvent Window Bool [EventMask] XEvent
  | SetSelectionOwner Bool Atom
  | ConvertSelection Selection
  | ChangeProperty Window Atom Atom Int PropertyMode String
  | FreeColors ColormapId [Pixel] Pixel{-planes-}
  | ReparentWindow Window {- parent -}
  | WarpPointer Point
  | SetRegion GCId Rect -- !!! modifies a GC -- cache problems!
  | AddToSaveSet
  | RemoveFromSaveSet
  | Bell Int
  | SetGCWarningHack {gcon,gcoff::PixmapId}
-- ONLY Pseudo commands below, add new Xlib command above this line !!!
  | GrabEvents Bool
  | UngrabEvents
  | TranslateEvent (XEvent -> Maybe XEvent) [EventMask]
  | ReparentToMe Path Window
  | GetWindowId -- move to XRequest?
  | SelectWindow Window
-- Layout pseudo commands
--  | LayoutMsg LayoutMessage
-- An I/O request to be passed through the top level (not anymore)
--  | DoIO Request
--  | DoXRequest XRequest
--
--  | DoXCommands [XCommand]
		-- Drawing speed hack, bypasses caches and resource management.
		-- Do not put any Alloc/Free, Create/Destroy, Grab/Ungrab type
		-- of commands inside DoXCommands!
--
  | MeButtonMachine -- tells menuPopupF where the buttons are
  deriving (Show,Read)

-- layoutRequestCmd = LayoutMsg . LayoutRequest

data XRequest
  = OpenDisplay DisplayName
  | CreateSimpleWindow Path Rect
  | CreateRootWindow Rect String -- resource name
  | CreateGC Drawable GCId GCAttributeList
  | LoadFont FontName
  | CreateFontCursor Int
  | GrabPointer Bool [EventMask]
  | LMLQueryFont FontId -- doesn't work in Haskell
  | AllocNamedColor ColormapId ColorName
  | AllocColor ColormapId RGB
  | CreatePixmap Size Depth
  | ReadBitmapFile FilePath
  | CreateBitmapFromData BitmapData
  | RmGetStringDatabase String
  | RmGetResource RmDatabase String String
  | TranslateCoordinates
  | InternAtom String Bool
  | GetAtomName Atom
  | GetWindowProperty Int Atom Bool Atom
  | QueryPointer
  | QueryFont FontId
  | LoadQueryFont FontName
  | QueryColor ColormapId Pixel
  | QueryTree
  | DefaultRootWindow
  | GetGeometry
  | DefaultVisual
  | Sync Bool -- discard::Bool
  | QueryTextExtents16 FontId String
  | ListFonts FontName Int -- pattern, maxnames
  | ListFontsWithInfo FontName Int -- pattern, maxnames
  | GetResource RmSpec
  | DbeQueryExtension
  | DbeAllocateBackBufferName SwapAction
  | DbeSwapBuffers SwapAction -- applies only to the fudget's own window.
  -- ONLY Pseudo requests below:
  | CreateMyWindow Rect
  deriving (Eq, Ord, Show, Read)

type Command = XCommand

data BitmapData = BitmapData Size (Maybe Point) [Int] 
                  deriving (Eq, Ord, Show, Read)

type DisplayName = String

-- Convenient abbreviations:
moveWindow (Point x y) = ConfigureWindow [CWX x, CWY y]
resizeWindow (Point w h) = ConfigureWindow [CWWidth w, CWHeight h]
moveResizeWindow (Rect (Point x y) (Point w h)) =
    ConfigureWindow [CWX x, CWY y, CWWidth w, CWHeight h]

clearWindowExpose = ClearArea (rR 0 0 0 0) True
                    -- Clears the window and generates appropriate
		    -- exposure events.See man page for XClearArea.
