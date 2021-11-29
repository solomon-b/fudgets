module ScoreF where
import Fudgets
import AllFudgets(appStorageF)

scoreF = placerF autoP (currentF>*<highF)>=^^<sumSP

highF = dispF "High:" >==<
        loopThroughRightF (toBothF >=^^< maxSP >=^< stripEither) storageF
  where
    storageF = appStorageF "SpaceInvaders.highscore" 0

currentF = dispF "Score:"

sumSP = accSP (\s->maybe 0 (s+)) 0
maxSP = accSP max 0

accSP f = mapstateSP (\s x->let x'=f s x in (x',[x']))

dispF lbl = lbl `labLeftOfF` intDispF' custom
  where custom = setFgColor "white" . setBgColor "black"
