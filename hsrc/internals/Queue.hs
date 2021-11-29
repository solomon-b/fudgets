module Queue(QUEUE, qmember, isempty, qremove, enter, empty) where

data QUEUE a = Queue [a] [a]  deriving (Eq, Ord)

empty = Queue [] []

enter (Queue outl' inl) x = Queue outl' (x : inl)

isempty (Queue [] []) = True
isempty _ = False

qremove (Queue (a : outl') inl) = Just (a, Queue outl' inl)
qremove (Queue [] []) = Nothing --error "qremove from empty queue"
qremove (Queue [] inl) = qremove (Queue (reverse inl) [])

qmember (Queue outl' inl) x = x `elem` outl' || x `elem` inl

