module ReadPic where
import Control.Applicative
import AllFudgets
import Pics
import BitmapOps

readPics (file1,file2) = (,) <$> readPic file1 <*> readPic file2

readPic name =
  case pics name of
    Bitmap (w,h) bytes ->
      do bmr <- Mk (bitmapFromData (BitmapData (Point w h) Nothing bytes))
         case bmr of
           BitmapReturn size _ pm -> return (PixmapImage size pm)
           BitmapBad -> error ("Can't read bitmap "++show name)
