module BranchF(branchF,branchFSP) where
import Direction
import Fudget
--import Path(Path(..))
import Route
import SP
--import Message(Message(..))

branchF :: (F (Path, a) b) -> (F (Path, a) b) -> F (Path, a) b
branchF (F f1) (F f2) = F{-ff-} (branchFSP f1 f2)

branchFSP :: (FSP (Path, a) b) -> (FSP (Path, a) b) -> FSP (Path, a) b
branchFSP f1 f2 =
    case f1 of
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (branchFSP f1' f2)
      PutSP msg f1' -> PutSP msg (branchFSP f1' f2)
      GetSP xf1 -> branchF1 xf1 f2
      NullSP -> restF2 f2

--and branchF1:: (FEvent *a->F *a *b) -> F *c *d -> F (Either *a *c) (Either *b *d)
branchF1 xf1 f2 =
    case f2 of
      PutSP (Low tcmd) f2' -> PutSP (compTurnRight tcmd) (branchF1 xf1 f2')
      PutSP msg f2' -> PutSP msg (branchF1 xf1 f2')
      GetSP xf2 -> GetSP (branchF12 xf1 xf2)
      NullSP -> GetSP (restF1x xf1)

--and branchF2:: F *a *b -> (FEvent *c->F *c *d) -> F (Either *a *c) (Either *b *d)
branchF2 f1 xf2 =
    case f1 of
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (branchF2 f1' xf2)
      PutSP msg f1' -> PutSP msg (branchF2 f1' xf2)
      GetSP xf1 -> GetSP (branchF12 xf1 xf2)
      NullSP -> GetSP (restF2x xf2)

--and branchF12:: (FEvent *a->F *a *b) -> (FEvent *c->F *c *d) -> FEvent (Either *a *c) -> F (Either *a *c) (Either *b *d)
branchF12 xf1 xf2 msg =
    case msg of
      High (L : path', x) -> branchF2 (xf1 (High (path', x))) xf2
      High (R : path', y) -> branchF1 xf1 (xf2 (High (path', y)))
      Low (L : path', x) -> branchF2 (xf1 (Low (path', x))) xf2
      Low (R : path', y) -> branchF1 xf1 (xf2 (Low (path', y)))
      _ -> GetSP (branchF12 xf1 xf2)

restF2 f2 =
    case f2 of
      PutSP (Low tcmd) f2' -> PutSP (compTurnRight tcmd) (restF2 f2')
      PutSP msg f2' -> PutSP msg (restF2 f2')
      GetSP xf2 -> GetSP (restF2x xf2)
      NullSP -> NullSP

restF2x xf2 msg =
    case msg of
      High (R : path', y) -> restF2 (xf2 (High (path', y)))
      Low (R : path', ev) -> restF2 (xf2 (Low (path', ev)))
      _ -> GetSP (restF2x xf2)

restF1 f1 =
    case f1 of
      PutSP (Low tcmd) f1' -> PutSP (compTurnLeft tcmd) (restF1 f1')
      PutSP msg f1' -> PutSP msg (restF1 f1')
      GetSP xf1 -> GetSP (restF1x xf1)
      NullSP -> NullSP

restF1x xf1 msg =
    case msg of
      High (L : path', x) -> restF1 (xf1 (High (path', x)))
      Low (L : path', ev) -> restF1 (xf1 (Low (path', ev)))
      _ -> GetSP (restF1x xf1)

