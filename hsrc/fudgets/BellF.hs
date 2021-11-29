module BellF where
import NullF(getF,putF)
import Command
import Xcommand

bellF = getF $ \ x ->
 	xcommandF (Bell 0) $
	putF x $
 	bellF
