module SimpleF(Drawer(..), Fms'(..), MapState(..), simpleF, simpleWindowF, simpleK) where
import Color
import Command
import XDraw
import Dlayout(windowF)
import DShellF
--import FDefaults
--import Event
import Fudget
import FudgetIO
import FRequest
--import Xcommand
import Gc
import Geometry(Size)
import LayoutRequest
--import Message(Message(..))
--import NullF
--import Spops
import MapstateK
import Xtypes

type MapState a b c = a -> b -> (a, c)

type Fms' a b c = MapState a (KEvent b) [KCommand c]

type Drawer = DrawCommand -> FRequest

simpleK k size s0 = simpleK' k size True True s0

simpleK' :: (Drawer->Drawer->Fms' a b c) -> Size -> Bool -> Bool -> a -> K b c
simpleK' k size fixedh fixedv s0 =
    allocNamedColorPixel defaultColormap "black" $ \fg ->
    allocNamedColorPixel defaultColormap "white" $ \bg ->
    wCreateGC rootGC [GCFunction GXcopy,GCForeground fg,GCBackground bg] $ \fgc ->
    wCreateGC fgc [GCForeground bg] $ \bgc ->
    putLow (layoutRequestCmd (plainLayout size fixedh fixedv)) $
    mapstateK (k (wDraw fgc) (wDraw bgc)) s0

simpleF title k size s0 =
    shellF title $
    simpleWindowF k size False False s0

simpleWindowF k size fh fv s0 =
    windowF [XCmd $ ChangeWindowAttributes [CWEventMask eventmask]] $
    simpleK' k size fh fv s0
  where
    eventmask = [ExposureMask, KeyPressMask, KeyReleaseMask, ButtonPressMask,
                 ButtonReleaseMask]
