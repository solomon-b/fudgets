module InvaderTypes where

data GEvent = Redraw | Key Action UpDown
data Action = MoveLeft | MoveRight | Fire
data UpDown = Up | Down
{-
data GCommand = Draw Pict
              | DrawOnTop Pict
              | ClearArea Rect Bool

data PlacedPict = PP Pict Point

data Pict = Object Object
          | Rectangles [Rect]
          | Text String

type Rect  = (Point,Size)
type Point = (Int,Int)
type Size  = Point
-}

data Phase = A | B  deriving (Eq,Show)
data Row = R1 | R2 | R3 deriving (Eq,Enum,Show)

rows = [R1,R2,R3]
phases f = (f A,f B)

rowNum :: Row -> Int
rowNum = succ . fromEnum

data Object = Vader Row Phase | VExplode Row | VShot Phase
            | Base | Explode | Ufo
            deriving (Eq,Show)
