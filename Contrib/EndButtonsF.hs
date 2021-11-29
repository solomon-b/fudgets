module EndButtonsF where
import AllFudgets

endButtonsF =
    noStretchF False True $ matrixF 2 (ok >+< cancel)
  where
    ok = buttonF "OK"
    cancel = buttonF' pm "Cancel"
      where
        pm = setKeys [([],"Escape")]


endButtonsF' =
    noStretchF False True $ matrixF 2 (ok >+< cancel) >=^< Left
  where
    ok = buttonF'' standard "OK" >=^< Left . setLabel >=^^< idempotSP
    cancel = buttonF' pm "Cancel"
      where
        pm = setKeys [([],"Escape")]
