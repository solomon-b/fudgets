module ConnectF where
import Fudgets(F,(>+<))

infixl :&:,>&<


--leaf :: (o->h)->F i o->(F i o,o->h,i->i)
tagF handler fud = TagF fud handler id

--data TagF a b c = TagF a b c
data TagF i o h t = TagF (F i o) (o->h) (t i)
-- TagF makes the type more readable but also more restrictive...

tf1 >&< tf2 = compTagF (>+<) tf1 tf2

compTagF compF (TagF fud1 get1 tag1) (TagF fud2 get2 tag2) =
    TagF (compF fud1 fud2) (either get1 get2) (etag1 :&: etag2)
  where
    etag1 = extend Left tag1
    etag2 = extend Right tag2

mapTF f (TagF fud get tag) = TagF (f fud) get tag

ltr ih (TagF fud get tag) =
  (fud,either get ih,Right :&: extend Left tag)

class Tag f where
  extend :: (b->c) -> f b -> f c

data Tags f1 f2 a = (f1 a) :&: (f2 a)

instance Tag ((->) a) where
  extend = (.)

instance (Tag f1,Tag f2) => Tag (Tags f1 f2) where
  extend f (g1:&:g2) = extend f g1:&:extend f g2

{-
newtype Selector d a = S (d a->Maybe a)

instance Tag (Selector d) where
  extend f 


f d1 d2 = (either d1 no,either no d2)
 :: (d1 a -> Maybe a) ->
    (d2 b -> Maybe b) ->
    (Either (d1 a) (d2 b)->Maybe a,Either (d1 a) (d2 b)->Maybe b)

a->Maybe b -> Either a c -> Maybe b
-}

no _ = Nothing
yes s = Just s
left f = either f no
right = either no
leftleft = left . left
leftyes = left yes
