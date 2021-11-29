module BgF(changeBackPixel, changeGetBackPixel) where
--import Color
import Command
--import Fudget
--import FudgetIO
--import Message(Message(..))
import Xcommand
--import NullF
import Xtypes
--import HaskellIO
import GCAttrs

--changeGetBackPixel :: ColorName -> Cont (K a b) Pixel
changeGetBackPixel bgcol f =
    --allocNamedColorDefPixel defaultColormap bgcol "white" $ \bgp ->
    convColorK [colorSpec bgcol,colorSpec "white"] $ \ bgp ->
    xcommandK (ChangeWindowAttributes [CWBackPixel bgp]) $
    xcommandK clearWindowExpose $
    f bgp

changeBackPixel bgcol = changeGetBackPixel bgcol . const
