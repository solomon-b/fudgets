module IsRequest where

import FRequest
import DialogueIO hiding (IOError)

isRequest c =
  case c of
    DReq (Select _) -> False
    DReq _ -> True
    XReq _ -> True
    SReq _ -> True
    _ -> False

isResponse e =
  case e of
    XEvt _ -> False
    LEvt _ -> False
    _ -> True
