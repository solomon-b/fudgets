module SmileyF where
import Fudgets

data SmileyMode = Sad | Indifferent | Happy deriving (Eq,Ord,Enum,Show)

smileyF = smileyF' standard

smileyF' :: Customiser (DisplayF SmileyMode) -> F SmileyMode void
smileyF' pm = displayF' pm'
  where pm' = pm .
	      setInitDisp Indifferent .
	      setInitSize Indifferent .
	      setStretchable (False,False) .
	      (setMargin 1 :: (Customiser (DisplayF SmileyMode)))

smileyD mode =
  FixD (Point 15 15) [
    drawCircle (Point 2 2) 2,	-- eye
    drawCircle (Point 10 2) 2,	-- eye
    DrawLine (lL 7 5 7 9),	-- nose
    case mode of
      Sad -> DrawArc (rR 3 11 8 4) 0 (64*180)		-- mouth
      Indifferent -> DrawLine (lL 3 12 11 12)		-- mouth
      Happy -> DrawArc (rR 3 9 8 4) (64*180) (64*180)	-- mouth
  ]

instance Graphic SmileyMode where
  measureGraphicK = measureGraphicK . smileyD
