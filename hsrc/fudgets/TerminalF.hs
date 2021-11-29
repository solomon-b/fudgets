module TerminalF(terminalF,cmdTerminalF,TerminalCmd(..)) where
import Spacer(marginF)
--import Alignment(Alignment(..))
import BgF
import Color
import Command
import FRequest
import DrawInWindow(wDrawImageString,wDrawImageString16,wCopyArea)
import XDraw
import Defaults(paperColor, fgColor)
import Dlayout(simpleGroupF, windowF)
import Event
import Font
import Fudget
--import FudgetIO
import Gc
import Geometry(Point(..), Rect(..), origin, pP, padd,)-- rectsize
import LayoutRequest
--import Placer(spacerF)
--import Spacers
import LoadFont
--import Message(Message(..))
import NullF
import StateMonads
--import EitherUtils(mapMaybe, stripMaybeDef)
import Xtypes
import CompOps
import GCAttrs() -- instances

grmarginF m f = simpleGroupF [] (marginF m f)

data TerminalCmd
  = TermText String -- add string on a new line
  | TermAppend String -- append string to last line
  | TermClear

terminalF :: FontName -> Int -> Int -> F String a
terminalF fname nrows ncols = cmdTerminalF fname nrows ncols >=^< TermText

cmdTerminalF :: FontName -> Int -> Int -> F TerminalCmd a
cmdTerminalF fname nrows ncols =
    let wattrs = [CWBackingStore WhenMapped, CWEventMask [ExposureMask]]
    in  grmarginF 2
                (windowF [XCmd $ ChangeWindowAttributes wattrs,
			  XCmd $ ConfigureWindow [CWBorderWidth 1]]
                         (terminalK fname nrows ncols))

terminalK fname nrows ncols =
    safeLoadQueryFont fname $ \fs ->
    allocNamedColorPixel defaultColormap fgColor $ \fg ->
    changeGetBackPixel paperColor $ \bg ->
    wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id fs), GCForeground fg, GCBackground bg]
				 (terminalK1 fs nrows ncols)

m1 $$$ m2 = m1>>m2

m1 $> xm2 = m1 >>= xm2

terminalK1 fs nrows ncols gc =
    let charsize@(Point charw charh) = string_box_size fs "M"
        startsize = curpos nrows ncols
        size = startsize
        curpos row col = pP (charw * col) (charh * row)
        drawpos row col = padd (curpos row col) (pP 0 (font_ascent fs))
	drimstr = if snd (font_range fs) > '\xff'
		  then wDrawImageString16
		  else wDrawImageString
        k =
            getKs $>
            (\msg ->
             (case msg of
                Low (XEvt (Expose _ 0)) -> redraw
                Low (LEvt (LayoutSize newsize)) -> setSize newsize
                Low _ -> nopMs
                High cmd -> case cmd of
		  TermText line -> addDrawLine line
		  TermAppend s -> appendDrawLine s
		  TermClear -> clearit) $$$
             k)
        drawline (r, l) =
            loadMs $>
            (\(lines', row, col, nrows', ncols') ->
             putLowMs (drimstr gc (drawpos r 0) l))
        redraw =
            loadMs $>
            (\(lines', row, col, nrows', ncols') ->
             putLowMs clearWindow $$$
             foldr (\l -> (drawline l $$$)) nopMs (zip [0 ..] (reverse lines')))
        setSize (Point x y) =
            loadMs $>
            (\(lines', row, col, nrows', ncols') ->
             let ncols'' = x `quot` charw
                 nrows'' = y `quot` charh
                 row' = row `min` nrows''
                 col' = col `min` ncols''
                 lines'' = take nrows'' lines'
             in  storeMs (lines'', row', col', nrows'', ncols'') $$$ redraw)
        addLine line =
            loadMs $>
            (\(lines', row, col, nrows', ncols') ->
             if row < nrows' - 1 then
                 let lines'' = line : lines'
                     row' = row + 1
                 in  storeMs (lines'', row', col, nrows', ncols')
             else
                 let lines'' = take nrows' (line : lines')
                 in  storeMs (lines'', row, col, nrows', ncols') $$$
                     putLowsMs [wCopyArea gc
                                           MyWindow
                                           (Rect (pP 0 charh)
                                                 (curpos (nrows' - 1) ncols'))
                                           origin,
                                clearArea  (Rect (curpos row 0)
                                                 (curpos 1 ncols'))
                                           False])
        appendLine s =
            loadMs $> \(lines', row, col, nrows', ncols') ->
	    case lines' of
	      []   -> storeMs ([s],row+1,col,nrows',ncols')
	      l:ls -> storeMs ((l++s):ls, row, col, nrows', ncols')
        clearit = loadMs $> \(lines, row, col, nrows, ncols) ->
		  storeMs ([],-1,0,nrows,ncols) $$$ redraw
        addDrawLine line =
            (addLine line $$$ loadMs) $> 
            (\(lines', row, col, nrows', ncols') -> drawline (row, line))
	appendDrawLine s =
	    (appendLine s $$$ loadMs) $>
            (\(line:_, row, col, nrows', ncols') -> drawline (row, line))
    in  putK (Low (layoutRequestCmd (plainLayout size False False))) $
        stateK ([], -1, 0, nrows, ncols) k nullK
