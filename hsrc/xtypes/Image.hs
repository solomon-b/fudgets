module Image where

newtype ImageFormat = ImageFormat Int deriving (Eq, Ord, Read, Show)

xyBitmap = ImageFormat 0
xyPixmap = ImageFormat 1
zPixmap  = ImageFormat 2
