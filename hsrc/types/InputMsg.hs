-- | Types for messages from buttons and input fields
module InputMsg where
import AuxTypes(KeySym(..))

-- | Button clicks
data Click = Click  deriving (Eq, Ord, Show)

-- | Output from dialog popups with OK and Cancel buttons
data ConfirmMsg = Confirm | Cancel  deriving (Eq, Ord, Show)

toConfirm (Left _)  = Confirm
toConfirm (Right _) = Cancel
fromConfirm Confirm = Left Click
fromConfirm Cancel  = Right Click

data InputMsg a = InputChange a |
                  InputDone KeySym a 
                  deriving (Eq, Ord, Show)

inputMsg = InputDone inputButtonKey
inputChange = InputChange

inputButtonKey = "." :: KeySym
inputLeaveKey  = ""  :: KeySym

stripInputMsg (InputDone _ x) = x
stripInputMsg (InputChange x) = x

tstInp p (InputChange s) = p s
tstInp p (InputDone k s) = p s

mapInp f (InputChange s) = InputChange (f s)
mapInp f (InputDone k s) = InputDone k (f s)

instance Functor InputMsg where fmap = mapInp

inputDone (InputDone k s) | k /= inputLeaveKey = Just s
inputDone _ = Nothing

inputLeaveDone (InputDone _ s) = Just s
inputLeaveDone _ = Nothing
