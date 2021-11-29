module Message where

data Message a b = Low a | High b  deriving (Eq, Ord, Show)

isHigh (High _) = True
isHigh _ = False

isLow (Low _) = True
isLow _ = False

stripHigh (High a) = Just a
stripHigh _ = Nothing

stripLow (Low b) = Just b
stripLow _ = Nothing

mapMessage fl fh' (Low l) = Low (fl l)
mapMessage fl fh' (High h) = High (fh' h)

message fl fh (Low l) = fl l
message fl fh (High h) = fh h

aLow f (Low l) = Low (f l)
aLow f (High h) = High h

aHigh f (High h) = High (f h)
aHigh f (Low l) = Low l

pushMsg (High xs) = fmap High xs
pushMsg (Low xs) = fmap Low xs

instance Functor (Message a) where
  fmap = aHigh
