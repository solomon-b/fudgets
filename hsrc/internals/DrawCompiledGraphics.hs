module DrawCompiledGraphics(drawK,drawK',drawChangesK,drawChangesK') where
import qualified DrawCompiledGraphics1 as D1
--import qualified DrawCompiledGraphics2 as D2
--import qualified DrawCompiledGraphics3 as D3
import DrawTypes(Drawable(MyWindow))
import CmdLineEnv(argReadKey)

drawK x = D1.drawK' MyWindow x
drawChangesK x = D1.drawChangesK' Nothing x

drawK' x = D1.drawK' x
drawChangesK' x = D1.drawChangesK' x
--drawK' = drawChoice D1.drawK' D2.drawK' D3.drawK'
--drawChangesK' = drawChoice D1.drawChangesK' D2.drawChangesK' D3.drawChangesK'


drawChoice =
  case choice of
    1 -> \ d1 d2 d3 -> d1
--  2 -> \ d1 d2 d3 -> d2
--  3 -> \ d1 d2 d3 -> d3
    _ -> error "unkown version of DrawCompiledGraphics"
  where
    choice = argReadKey "draw" (1::Int)
