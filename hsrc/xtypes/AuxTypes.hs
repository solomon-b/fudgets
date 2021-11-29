-- | Auxiliary Xlib types
module AuxTypes where

data Gravity = ForgetGravity |
               NorthWestGravity |
               NorthGravity |
               NorthEastGravity |
               WestGravity |
               CenterGravity |
               EastGravity |
               SouthWestGravity |
               SouthGravity |
               SouthEastGravity |
               StaticGravity 
               deriving (Eq, Ord, Read, Show, Bounded, Enum)

data ShapeKind = ShapeBounding | ShapeClip
                 deriving (Eq, Ord, Show, Read, Bounded, Enum)

data ShapeOperation = ShapeSet |
                      ShapeUnion |
                      ShapeIntersect |
                      ShapeSubtract |
                      ShapeInvert 
                      deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- There already is an Ordering in the 1.3 Prelude
data Ordering' = Unsorted | YSorted | YXSorted | YXBanded
     deriving (Eq, Ord, Show, Read, Bounded, Enum)

type RmClass = String
type RmName = String
type RmQuery = (RmClass, RmName)
type RmSpec = [RmQuery]
type RmValue = String
type RmDatabase = Int

rmNothing = 0::Int

data Modifiers = Shift | Lock | Control
               | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
               | Button1 | Button2 | Button3 | Button4 | Button5
               | Mod13 | Mod14 -- non-standard, but used in XQuartz
               | Any 
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

clModifiers =
    [Shift, Lock, Control, Mod1, Mod2, Mod3, Mod4, Mod5,
     Button1, Button2, Button3, Button4, Button5]

data Button = AnyButton | Button Int  deriving (Eq, Ord, Read, Show)

type ModState = [Modifiers]

type KeySym = String

