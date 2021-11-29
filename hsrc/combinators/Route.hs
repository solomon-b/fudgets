module Route(compTurnRight, compTurnLeft, compPath) where
import Direction
import Message(Message(..))
import Path({-Path(..),-}path, turn)

compPath (tag, ev) wrongaddr c =
    case path tag of
      (L, tag') -> c $ Left (Low (tag', ev))
      (R, tag') -> c $ Right (Low (tag', ev))
      _ -> wrongaddr

compTurnLeft  (tag, cmd) = Low (turn L tag, cmd)
compTurnRight (tag, cmd) = Low (turn R tag, cmd)
