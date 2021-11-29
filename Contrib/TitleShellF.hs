module TitleShellF where
import AllFudgets

titleShellF = titleShellF' standard

--titleShellF' :: Customiser ShellF -> String -> F (Either () i) (Either String o) -> F i o

titleShellF' pm title fud =
    filterRightSP >^^=< wmShellF' pm' title fud >=^< mapEither Left id
  where
    pm' = pm . setDeleteWindowAction (Just DeleteQuit)

wmShellF = wmShellF' standard

wmShellF' pm0 title fud =
    shellKF' pm titleK0 fud
  where pm = setDeleteWindowAction Nothing . pm0
	action = maybe (Just reportK)
	               (fmap action')
		       (getDeleteWindowActionMaybe' pm0)
	  where
	    action' DeleteQuit = exitK
	    action' DeleteUnmap = unmapWindowK

        titleK0 = startupK [High (Left title)] titleK
        titleK = wmK action
