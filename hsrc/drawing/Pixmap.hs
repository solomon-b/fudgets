module Pixmap(bitmapFromData, readBitmapFile, createPixmap) where
import Command
import Event
--import Font(FontStruct)
--import Fudget
--import Geometry(Line, Point, Rect, Size(..))
--import LayoutRequest(LayoutRequest)
--import Message(Message(..))
--import Path(Path(..))
import Xrequest
--import Xtypes

createPixmap size depth =
    let cmd = CreatePixmap size depth
        expected (PixmapCreated pixmap) = Just pixmap
        expected _ = Nothing
    in  xrequest cmd expected

readBitmapFile name =
    let cmd = ReadBitmapFile name
        expected (BitmapRead b) = Just b
        expected _ = Nothing
    in  xrequest cmd expected

bitmapFromData bd =
    let cmd = CreateBitmapFromData bd
        expected (BitmapRead b) = Just b
        expected _ = Nothing
    in  xrequest cmd expected

