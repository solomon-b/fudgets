module ResourceIds where
import Data.Word(Word32)

newtype XID = XID Word32 deriving (Eq,Ord)

instance Show XID where showsPrec d (XID w32) = showsPrec d w32
instance Read XID where readsPrec d s = [(XID w,r)|(w,r)<-readsPrec d s]

newtype WindowId = WindowId XID deriving (Eq, Ord, Show, Read)

type Window = WindowId
type XWId = WindowId

rootWindow = WindowId (XID 0)
noWindow = WindowId (XID (-1))
windowNone = WindowId (XID 0)

newtype PixmapId = PixmapId XID deriving (Eq, Ord, Show, Read)
newtype DbeBackBufferId = DbeBackBufferId XID deriving (Eq, Ord, Show, Read)
newtype FontId = FontId XID deriving (Eq, Ord, Read, Show)
newtype GCId = GCId Int deriving (Eq, Ord, Read, Show)
newtype CursorId = CursorId XID deriving (Eq, Ord, Read, Show)
newtype ColormapId = ColormapId XID deriving (Eq, Ord, Read, Show)

defaultColormap = ColormapId (XID 0)
cursorNone = CursorId (XID 0)

newtype Atom = Atom Word32 deriving (Eq, Ord, Show, Read)


type ColorName = String
type FontName = String

type Time = Int
currentTime = 0::Time

type Depth = Int

copyFromParent = 0 :: Depth
parentRelative = PixmapId (XID 1)
none = PixmapId (XID 0)

rootGC = GCId 0

