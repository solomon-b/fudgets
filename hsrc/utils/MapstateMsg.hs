module MapstateMsg(mapLow, mapHigh, mapstateLow, mapstateHigh) where
import Message(Message(..))
--import SP(SP)
import Spops
import HbcUtils(apSnd)

mapstateHigh p s =
    let ms s' (Low msg) = (s', [Low msg])
        ms s' (High msg) = apSnd (map High) (p s' msg)
    in  mapstateSP ms s

mapstateLow p s =
    let ms s' (High msg) = (s', [High msg])
        ms s' (Low msg) = apSnd (map Low) (p s' msg)
    in  mapstateSP ms s

mapHigh p =
    let ms (High msg) = map High (p msg)
        ms (Low msg) = [Low msg]
    in  concmapSP ms

mapLow p =
    let ms (Low msg) = map Low (p msg)
        ms (High msg) = [High msg]
    in  concmapSP ms

