module TextExtents where
import Xrequest(xrequestK)
import Command
import Event

queryTextExtents16K fid str k =
    xrequestK cmd expected $ \ (a,d,cs) -> k a d cs
  where
    cmd = QueryTextExtents16 fid str

    expected (TextExtents16Queried a d cs) = Just (a,d,cs)
    expected _ = Nothing
