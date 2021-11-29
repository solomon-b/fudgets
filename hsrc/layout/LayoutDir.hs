module LayoutDir where
import Geometry(Point(..), xcoord, ycoord)
import LayoutRequest
import Utils(swap)

data Orientation = Above | Below | RightOf | LeftOf
                   deriving (Eq, Ord, Show)

data LayoutDir = Horizontal | Vertical  deriving (Eq, Ord, Show)


xc Horizontal = xcoord
xc Vertical = ycoord

yc Horizontal = ycoord
yc Vertical = xcoord

fixh Horizontal = fixedh
fixh Vertical = fixedv

fixv Horizontal = fixedv
fixv Vertical = fixedh

mkp Horizontal x y = Point x y
mkp Vertical x y = Point y x

vswap Horizontal = id
vswap Vertical = swap

colinear Horizontal h v = h
colinear Vertical   h v = v

orthogonal Horizontal h v = v
orthogonal Vertical   h v = h

