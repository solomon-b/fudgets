module Cursor where
import Command
import Event
import Fudget
import Xrequest
import Xcommand
import Xtypes

createFontCursor shape =
    let cmd = CreateFontCursor shape
        expected (CursorCreated cursor) = Just cursor
        expected _ = Nothing
    in  xrequestK cmd expected

setFontCursor :: Int -> K a b -> K a b
setFontCursor shape process =
    createFontCursor shape $ \ cursor ->
    xcommandK (ChangeWindowAttributes [CWCursor cursor]) $
    process

defineCursor cursor = xcommandK (ChangeWindowAttributes [CWCursor cursor])
undefineCusror = xcommandK (ChangeWindowAttributes [CWCursor cursorNone])
