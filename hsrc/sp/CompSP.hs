module CompSP(prepostMapSP, postMapSP, preMapSP, idRightSP, idLeftSP, idHighSP,
              idLowSP, compMsgSP, compSP, compEitherSP, serCompSP) where
import Message(Message(..))
import SP
import Spops

--serCompSP :: SP b c -> SP a b -> SP a c
serCompSP sp1 sp2 =
    case sp1 of
      PutSP y sp1' -> PutSP y (serCompSP sp1' sp2)
      GetSP xsp1 -> serCompSP1 xsp1 sp2
      NullSP -> NullSP

serCompSP1 xsp1 sp2 =
    case sp2 of
      PutSP y sp2' -> serCompSP (xsp1 y) sp2'
      GetSP xsp2 -> GetSP (serCompSP1 xsp1 . xsp2)
      NullSP -> NullSP

---

compSP = compEitherSP

--compEitherSP :: SP a1 b1 -> SP a2 b2 -> SP (Either a1 a2) (Either b1 b2)
compEitherSP sp1 sp2 =
    case sp1 of
      PutSP y sp1' -> PutSP (Left y) (compEitherSP sp1' sp2)
      GetSP xsp1 -> compEitherSP1 xsp1 sp2
      NullSP -> rEitherSP sp2

--and compEitherSP1 :: (*a1->SP *a1 *b1) -> SP *a2 *b2 -> SP (Either *a1 *a2) (Either *b1 *b2)
compEitherSP1 xsp1 sp2 =
    case sp2 of
      PutSP y sp2' -> PutSP (Right y) (compEitherSP1 xsp1 sp2')
      GetSP xsp2 -> compEitherSP12 xsp1 xsp2
      NullSP -> lEitherSP (GetSP xsp1)

--and compEitherSP2 :: (SP *a1 *b1) -> (*a2->SP *a2 *b2) -> SP (Either *a1 *a2) (Either *b1 *b2)
compEitherSP2 sp1 xsp2 =
    case sp1 of
      PutSP y sp1' -> PutSP (Left y) (compEitherSP2 sp1' xsp2)
      GetSP xsp1 -> compEitherSP12 xsp1 xsp2
      NullSP -> rEitherSP (GetSP xsp2)

--and compEitherSP12 :: (*a1->SP *a1 *b1) -> (*a2->SP *a2 *b2) -> SP (Either *a1 *a2) (Either *b1 *b2)
compEitherSP12 xsp1 xsp2 =
    GetSP (\x ->
           case x of
             Left a -> compEitherSP2 (xsp1 a) xsp2
             Right b -> compEitherSP1 xsp1 (xsp2 b))

lEitherSP sp1 =
    case sp1 of
      PutSP y sp1' -> PutSP (Left y) (lEitherSP sp1')
      GetSP xsp1 -> lEitherSP1 xsp1
      NullSP -> NullSP

lEitherSP1 xsp1 =
    GetSP (\x ->
           case x of
             Left a -> lEitherSP (xsp1 a)
             Right b -> lEitherSP1 xsp1)

rEitherSP sp2 =
    case sp2 of
      PutSP y sp2' -> PutSP (Right y) (rEitherSP sp2')
      GetSP xsp2 -> rEitherSP2 xsp2
      NullSP -> NullSP

rEitherSP2 xsp2 =
    GetSP (\x ->
           case x of
             Right a -> rEitherSP (xsp2 a)
             Left b -> rEitherSP2 xsp2)

---
--and preMapSP :: SP *b *c -> (*a->*b) -> SP *a *c
preMapSP sp pre =
    case sp of
      PutSP y sp' -> PutSP y (preMapSP sp' pre)
      GetSP xsp -> GetSP (\x -> preMapSP (xsp (pre x)) pre)
      NullSP -> NullSP

--and postMapSP :: (*b->*c) -> SP *a *b -> SP *a *c
postMapSP post sp =
    case sp of
      PutSP y sp' -> PutSP (post y) (postMapSP post sp')
      GetSP xsp -> GetSP (\x -> postMapSP post (xsp x))
      NullSP -> NullSP

prepostMapSP pre post sp =
    case sp of
      PutSP y sp' -> PutSP (post y) (prepostMapSP pre post sp')
      GetSP xsp -> GetSP (\x -> prepostMapSP pre post (xsp (pre x)))
      NullSP -> NullSP

---
idLowSP sp = compMsgSP idSP sp

idHighSP sp = compMsgSP sp idSP

idLeftSP sp = compEitherSP idSP sp

idRightSP sp = compEitherSP sp idSP

---
--and compMsgSP :: SP *a1 *b1 -> SP *a2 *b2 -> SP (Message *a1 *a2) (Message *b1 *b2)
-- compMsgSP was constructed from compEitherSP with some global substitutions
compMsgSP sp1 sp2 =
    case sp1 of
      PutSP y sp1' -> PutSP (Low y) (compMsgSP sp1' sp2)
      GetSP xsp1 -> compMsgSP1 xsp1 sp2
      NullSP -> rMsgSP sp2

compMsgSP1 xsp1 sp2 =
    case sp2 of
      PutSP y sp2' -> PutSP (High y) (compMsgSP1 xsp1 sp2')
      GetSP xsp2 -> compMsgSP12 xsp1 xsp2
      NullSP -> lMsgSP (GetSP xsp1)

compMsgSP2 sp1 xsp2 =
    case sp1 of
      PutSP y sp1' -> PutSP (Low y) (compMsgSP2 sp1' xsp2)
      GetSP xsp1 -> compMsgSP12 xsp1 xsp2
      NullSP -> rMsgSP (GetSP xsp2)

compMsgSP12 xsp1 xsp2 =
    GetSP (\x ->
           case x of
             Low a -> compMsgSP2 (xsp1 a) xsp2
             High b -> compMsgSP1 xsp1 (xsp2 b))

lMsgSP sp1 =
    case sp1 of
      PutSP y sp1' -> PutSP (Low y) (lMsgSP sp1')
      GetSP xsp1 -> lMsgSP1 xsp1
      NullSP -> NullSP

lMsgSP1 xsp1 =
    GetSP (\x ->
           case x of
             Low a -> lMsgSP (xsp1 a)
             High b -> lMsgSP1 xsp1)

rMsgSP sp2 =
    case sp2 of
      PutSP y sp2' -> PutSP (High y) (rMsgSP sp2')
      GetSP xsp2 -> rMsgSP2 xsp2
      NullSP -> NullSP

rMsgSP2 xsp2 =
    GetSP (\x ->
           case x of
             High a -> rMsgSP (xsp2 a)
             Low b -> rMsgSP2 xsp2)

