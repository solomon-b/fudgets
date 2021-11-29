module ParSP(seqSP, parSP) where
import SP

parSP sp1 sp2 =
    case sp1 of
      PutSP y sp1' -> PutSP y (parSP sp1' sp2)
      GetSP xsp1 -> parSP1 xsp1 sp2
      NullSP -> sp2

parSP1 xsp1 sp2 =
    case sp2 of
      PutSP y sp2' -> PutSP y (parSP1 xsp1 sp2')
      GetSP xsp2 -> GetSP (\x -> parSP (xsp1 x) (xsp2 x))
      NullSP -> GetSP xsp1

seqSP sp1 sp2 =
    case sp1 of
      PutSP y sp1' -> PutSP y (seqSP sp1' sp2)
      GetSP xsp -> GetSP (\x -> seqSP (xsp x) sp2)
      NullSP -> sp2

