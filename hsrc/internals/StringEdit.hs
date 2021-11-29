module StringEdit(
       moveCursorHome,moveCursorEnd,extendCursorHome,extendCursorEnd,
       moveCursorRight, moveCursorLeft, extendCursorLeft,extendCursorRight,
       deleteItemRight, deleteItemLeft, deleteToEnd, deleteToHome,
       insertItem, insertItemsSelected, showField, getField,
       createField2, createField, Field) where

data Field a = F [a] [a] [a]  deriving (Eq, Ord)

createField s = F [] s []

createField2 (before, after) = F (reverse before) [] after

getField (F l c r) = reverse l ++ c ++ r

showField show' show_cursor (F l c r) =
    show' r . show_cursor c . show' (reverse l)

insertItem (F l c r) i = F (i : l) [] r
insertItemsSelected (F l c r) i = F l i r

deleteItemLeft f@(F [] [] _) = f
deleteItemLeft (F (i : l) [] r) = F l [] r
deleteItemLeft (F l c r) = F l [] r

deleteItemRight f@(F _ [] []) = f
deleteItemRight (F l [] (i : r)) = F l [] r
deleteItemRight (F l c r) = F l [] r

deleteToEnd (F l c r) = F l [] []
deleteToHome (F l c r) = F [] [] r

extendCursorRight (F l c (i : r)) = F l (c ++ [i]) r
extendCursorRight f@(F _ _ []) = f

extendCursorLeft (F (i : l) c r) = F l (i : c) r
extendCursorLeft f@(F [] _ _) = f

moveCursorLeft f@(F [] [] _) = f
moveCursorLeft (F (i : l) [] r) = F l [] (i : r)
moveCursorLeft (F l c r) = F l [] (c ++ r)

moveCursorRight f@(F _ [] []) = f
moveCursorRight (F l [] (i : r)) = F (i : l) [] r
moveCursorRight (F l c r) = F (reverse c ++ l) [] r

moveCursorHome f = F [] [] (getField f)
moveCursorEnd  f = F (reverse (getField f)) [] []

extendCursorHome (F l c r) = F [] (reverse l++c) r
extendCursorEnd (F l c r) = F l (c++r) []
