module ListRequest where

default (Int)

data ListRequest a
  = ReplaceItems Int Int [a] -- from, how many, new lines
  | HighlightItems [Int]     -- replaces list of highlighted lines
  | PickItem Int            -- as if user clicked on a line
listEnd = -1

replaceAll          = ReplaceItems 0    listEnd           -- replace all text
replaceAllFrom from = ReplaceItems from listEnd
deleteItems from cnt = ReplaceItems from cnt     []
insertItems from     = ReplaceItems from 0
appendItems          = ReplaceItems listEnd listEnd
changeItems from txt = ReplaceItems from (length txt) txt

replaceItems = ReplaceItems
highlightItems = HighlightItems
pickItem = PickItem

--applyListRequest (ReplaceItems 0 cnt newtxt) oldtxt | cnt==listEnd = newtxt
applyListRequest (ReplaceItems from cnt newtxt) oldtxt =
  let before = if from==listEnd
               then oldtxt
	       else take from oldtxt
      after  = if from==listEnd || cnt==listEnd
               then []
	       else drop (from+cnt) oldtxt
  in before++newtxt++after
applyListRequest _ oldtxt = oldtxt
