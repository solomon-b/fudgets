module MapstateK where
import Fudget
import Spops(mapstateSP)

mapstateK f s = K $ mapstateSP f s
