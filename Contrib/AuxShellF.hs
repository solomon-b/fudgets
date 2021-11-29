module AuxShellF where
import Fudgets
import TitleShellF(wmShellF')

auxShellF = auxShellF' standard
delayedAuxShellF = delayedAuxShellF' standard

delayedAuxShellF' pm title fud =
    auxShellF' pm title fud >=^^< emptyDownSP
  where
    -- 3x3 state transition table:
    emptyDownSP = getPopSP emptyDownSP upSP      downSP
    downSP    x = getPopSP (downSP x)  (msgSP x) downSP
    upSP        = getPopSP emptyDownSP upSP      msgSP

    msgSP x = putSP (Right x) upSP

    getPopSP downSP upSP msgSP = getSP $ either popSP msgSP
      where popSP b = putSP (Left b) $ if b then upSP else downSP

auxShellF' pm title fud =
  mapEither (const False) id >^=<
  wmShellF' (pm . setVisible False) title fud
  >=^< mapEither Right id

{-
auxShellF lbl title fud =
  loopThroughRightF
    (wmShellF' (setVisible False) title fud)
    (Right>^=<toggleButtonF lbl>=^<const False)
-}
