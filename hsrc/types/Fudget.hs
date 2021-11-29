module Fudget(
  module Fudget,SP,
  --XCommand,XEvent,
  FRequest,FResponse,
  Message(..),Path(..),Direction) where
--import Command(XCommand)
--import Event(XEvent)
import FRequest(FRequest,FResponse)
import Message(Message(..))
import Direction(Direction)
import Path(Path(..))
import SP(SP)

type TEvent   = (Path, FResponse)
type TCommand = (Path, FRequest)

type Fa a b c d = SP (Message a c) (Message b d)

type FEvent a = Message TEvent a
type KEvent a = Message FResponse a

type FCommand a = Message TCommand a
type KCommand a = Message FRequest a

type Fudget a b = F a b

-- Traditional definitions:
--type F a b = Fa TEvent TCommand a b
--type K a b = Fa XEvent XCommand a b

-- New definitions:

type FSP hi ho = SP (FEvent hi) (FCommand ho)     --  = old def of F hi ho
type KSP hi ho = SP (KEvent hi) (KCommand ho)     --  = old def of K hi ho

newtype F hi ho = F (FSP hi ho)
newtype K hi ho = K (KSP hi ho)
--data F hi ho = F (FSP hi ho)
--data K hi ho = K (KSP hi ho)

kk=K
ff=F
unK (K sp) = sp
unF (F sp) = sp
