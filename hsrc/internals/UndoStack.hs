module UndoStack(UndoStack,undoStack,doit,undo,redo) where
--import EitherUtils(mapMaybe)

data UndoStack a = U (Maybe Int) [a] [a] [a]
--                   depth limit undos temp-undos temp-redos
undoStack:: Maybe Int -> UndoStack a
undoStack on = U (fmap (\n -> max 0 n + 1) on) [] [] []

otake on l = case on of
	        Nothing -> l
		Just n -> take n l

doit :: UndoStack a -> a -> (UndoStack a -> c) -> c
doit (U on u tu tr) a c = seq (length u') $ c (U on u' (tail u') [])
    where u' = otake on (a:take 1 tr++u)

undo :: UndoStack a -> Maybe (a,UndoStack a)
undo (U on u tu tr) = case tu of
			a:tu' -> Just (a,U on u tu' (a:tr))
			[] -> Nothing

redo :: UndoStack a -> Maybe (a,UndoStack a)
redo (U on u tu tr) = 
     case tr of
       a:b:tr' -> Just (b,U on u (a:tu) (b:tr'))
       [a] -> Just (head u,U on u (a:tu) [])
       [] -> Nothing
