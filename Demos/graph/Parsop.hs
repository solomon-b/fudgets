module	Parsop where
infixl |||, +++, +>+, +<+
{-
infix "==>";
prefix "***";
infix "+<+";
infix "+>+";
infix "iff";
prefix "%%";
-}
{-
export	empty,
-- matches the empty string
	+++,
-- sequence, returns a pair
	|||,
-- alternative
	is,
-- terminal tested with a predicate
	`,
-- terminal tested with equality
	***,
-- 0 or many (transitive closure)
	+<+,
-- sequence, throw away first part
	+>+,
-- sequence, throw away second part
	==>,
-- transform
	cond,
-- 0 or one
	seq',
-- sequence of 0 or many with a separator
	seq1,
-- sequence of 1 or many with a separator
	iff,
-- conditional
	%%;
-- uncurry
-}
trycase pf fail succeed ws=
  case pf ws of
     Left _ -> fail ws
     Right(x,ws) -> succeed x ws


failure ws= Left ws
success x ws = Right(x,ws)

try pf cont = trycase pf failure cont

is p [] = Left []
is p (w:ws) = if p w
		  then Right(w,ws)
		  else Left (w:ws)

lit w = is (==w)

pf1 +++ pf2 = try pf1 (\x ->try pf2 (\y -> success(x,y)))

pf1 ||| pf2 = trycase pf1 pf2 success

pf ==> f = try pf (success . f)

(***) pf = ((pf +++ (***) pf) ==> (%%) (:)) ||| success []

sep +<+ pf1 = (sep +++ pf1) ==> snd
pf1 +>+ sep = (pf1 +++ sep) ==> fst

--seq1 pf sep = ((pf +++ (sep +<+ seq1 pf sep)) ==> %% (.)) ||| (pf ==> (:[]))
seq1 pf sep = (pf +++ ((sep +<+ seq1 pf sep) ||| empty [])) ==> (%%) (:)

seq' pf sep = seq1 pf sep ||| empty []

empty = success

cond pf x = pf ||| empty x

pf `iff` p = try pf (\x -> if p x
			  then success x
			  else failure)

(%%) = uncurry
