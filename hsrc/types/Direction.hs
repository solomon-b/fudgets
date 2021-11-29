module Direction(Direction(..)) where

data Direction = L | R | Dno Int  deriving (Eq, Ord, Read, Show)
