module Event(module Event,FontStructList,FontStruct) where
import Font(FontStruct,FontStructList,CharStruct)
import Visual(Visual)
import Geometry
import Xtypes
--import DialogueIO hiding (IOError)

newtype KeyCode = KeyCode Int deriving (Eq, Ord, Show, Read)

data Pressed = Pressed | Released | MultiClick Int
               deriving (Eq, Ord, Show, Read)

data Detail = NotifyAncestor |
              NotifyVirtual |
              NotifyInferior |
              NotifyNonlinear |
              NotifyNonlinearVirtual |
              NotifyPointer |
              NotifyPointerRoot |
              NotifyDetailNothing 
              deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Mode = NotifyNormal |
            NotifyGrab |
            NotifyUngrab |
            NotifyWhileGrabbed 
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Visibility = VisibilityUnobscured |
                  VisibilityPartiallyObscured |
                  VisibilityFullyObscured 
                  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ClientData = Byte String |
                  Short [Int] |
                  Long [Int] 
                  deriving (Eq, Ord, Show, Read)

data XEvent
  = FocusIn   { detail::Detail, mode::Mode }
  | FocusOut  { detail::Detail, mode::Mode }
  | KeymapNotify
  | GraphicsExpose { rect::Rect, count::Int, major_code, minor_code::Int}
  | KeyEvent { time::Time, pos,rootPos::Point, state::ModState, type'::Pressed, keycode::KeyCode, keySym::KeySym, keyLookup::KeyLookup }
  | ButtonEvent { time::Time, pos,rootPos::Point, state::ModState, type'::Pressed, button::Button}
  | MotionNotify { time::Time, pos,rootPos::Point, state::ModState }
  | EnterNotify  { time::Time, pos,rootPos::Point, detail::Detail, mode::Mode, focus::Bool }
  | LeaveNotify  { time::Time, pos,rootPos::Point, detail::Detail, mode::Mode, focus::Bool }
  | Expose {rect::Rect, count::Int}
  | NoExpose
  | VisibilityNotify Visibility
  | CreateNotify Window
  | DestroyNotify Window
  | UnmapNotify Window
  | MapNotify Window
  | MapRequest Window
  | ReparentNotify
  | ConfigureNotify Rect Int
  | ConfigureRequest
  | GravityNotify
  | ResizeRequest Point
  | CirculateNotify
  | CirculateRequest
  | PropertyNotify
  | SelectionClear Atom
  | SelectionRequest Time Window Selection
  | SelectionNotify Time Selection
  | ColormapNotify
  | ClientMessage Atom ClientData
  | MappingNotify
  -- Pseudo event below:
--  | IOResponse Response
--
--  | LayoutPlace Rect
--  | LayoutSize Size
--  | LayoutPos Point -- Position in parent window. Occationally useful.
  | YourWindowId Window
  | MenuPopupMode Bool -- used by buttonmachine to adjust its behaviour
  deriving (Show,Read)

type Event = XEvent

data XResponse
  = DisplayOpened Display
  | WindowCreated Window
  | GCCreated GCId
  | CursorCreated CursorId
  | PointerGrabbed GrabPointerResult
  | FontLoaded FontId
  | LMLFontQueried FontStruct
  | ColorAllocated (Maybe Color)
  | PixmapCreated PixmapId
  | BitmapRead BitmapReturn
  | RmDatabaseCreated RmDatabase
  | GotResource (Maybe (String, RmValue))
  | CoordinatesTranslated Point
  | GotAtom Atom
  | GotAtomName (Maybe String)
  | GotEvent (Window, XEvent)
  | GotWindowProperty Atom Int Int Int String
  | PointerQueried Bool Point Point ModState
  | FontQueried (Maybe FontStructList)
  | ColorQueried Color
  | TreeQueried Window Window [Window]  -- root parent children
  | GotDefaultRootWindow Window
  | GotGeometry Rect Int Int
  | GotVisual Visual
  | Synced
  | TextExtents16Queried Int Int CharStruct -- ascent descent overall
  | GotFontList [FontName]
  | GotFontListWithInfo [(FontName,FontStructList)]
--  | GotFontListWithInfo [(FontStructList)]
  | DbeExtensionQueried Int Int Int -- status (/=0 means ok), major, minor
  | DbeBuffersSwapped Int -- status (useless?)
  | DbeBackBufferNameAllocated DbeBackBufferId
  deriving (Show,Read)

data BitmapReturn
  = BitmapBad
  | BitmapReturn Size (Maybe Point) PixmapId 
  deriving (Eq, Ord, Show, Read)

