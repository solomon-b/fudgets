-- | The entire world, implemented as a single fudget, instead of one fudget
-- per object in the world.
module WorldF(worldF) where
import System.Random
import Control.Applicative
import Control.Monad(when)
import Data.List(nub)
import ReactionM
import GUI
import InvaderTypes hiding (Phase(..))

data World    = W {rnd::StdGen, shelters::Shelters, base::Base, ufo::Ufo,
                   torpedo::Torpedo, vshots::VShots, invaders::Invaders}
data Base     = B {bPict,exbPict::Pict, bPos::Point, lives::Int, bSpeed::Speed}
              | ExB {bPict,exbPict::Pict, bPos::Point, lives::Int, exbAge::Int}
--data Shelters = Sh [PlacedPict] -- hmm
type Shelters = [Shelter]
data Shelter  = Sh {shGC::GCId,shBR::Rect,shRs::[Rect]}
data Torpedo  = T Pict (Maybe Point)
data Ufo      = U Pict GCId UfoState
data UfoState = Wait Ticks | Moving Score Speed Point | Hit Ticks Score Point
data VShots   = V (Pict,Pict) [Point]
data VShot    = V1 (Pict,Pict) Point
data Invaders = Invs {invs::[Invader], level::Int, step::Speed, delay::Ticks}
data Invader  = Inv {invPict::(Pict,Pict),exvPict::Pict,
                     invScore::Int,invPos::Point}
              | Exv {exvPict::Pict, invPos::Point} -- exploded
type ExVader  = Pict
type Speed = Int
type Ticks = Int
type Score = Int

modBase = update . updateBase
updateBase f w@W{base=b} = w{base=f b}
--updateTorpedo f =w@W{torpedo=t} = w{torpedo=f b}

exBase (B pm ex p l v) = ExB pm ex p l 0
exBase base = base

alive w = case base w of B {} -> True; _-> False

spare pm n = if n>=0
             then hboxD (replicate n (atomicD (Right pm)))
             else atomicD (Left "Game over!")

--------------------------------------------------------------------------------

worldF g space = spaceF space (initWorldK g space) (objectsK space)

--------------------------------------------------------------------------------

initWorldK g space =
  W g <$> initSheltersK space <*> initBaseK space <*> initUfoK
      <*> initTorpedoK <*> initVShotsK <*> initInvadersK

initSheltersK space =
    do gc <- objGC shelterColor
       let sh p = Sh gc (Rect p s) (move p shape)
       return $ map sh ps
  where
    s@(Point w h) = shelterSize
    m = (xcoord space-3*3-7*w) `div` 2
    y = ycoord space-margin-3*h
    xs = [m+2*(w+3)*i|i<-[0..3]]
    ps = [Point x y|x<-xs]

    shape = [v x roofy h|x<-[0..roofx-1]]++
            [v x 0     h|x<-[roofx..legx-1]]++
            [v x 0 (h-legy)|x<-[legx..w-legx-1]]++
            [v x 0     h|x<-[w-legx..w-roofx-1]]++
            [v x roofy h|x<-[w-roofx..w-1]]
    v x y1 y2 = rR x y1 1 (y2-y1)
    legx = w `div` 3 - 1
    legy = h `div` 4
    roofy = legy
    roofx = w `div` 10

initBaseK space =
  do pict <- readObjPict Base
     ex   <- readObjPict Explode
     let startpos = (baseStartPos space pict){xcoord= -width pict}
         lives = 3
     putLivesMkc (spare pict lives)
     return (ExB pict ex startpos (lives+1) 50)

baseStartPos space pict = Point margin (ycoord space-height pict-margin)

initUfoK =
  do gc <- objGC ufoColor
     pict <- readPict gc Ufo
     return $ U pict gc (Wait ufoDelay)
  
initTorpedoK =
  do gc <- objGC shotColor
     let pict = rectangles gc torpedoSize [Rect 0 torpedoSize]
     return (T pict Nothing)

initVShotsK = flip V [] <$> readObjPicts (phases VShot)

initInvadersK =
    invaders <$> mapM (readObjPicts . phases . Vader) rows
             <*> mapM (readObjPict . VExplode) rows
  where
    invaders is es = Invs invs startLevel dx0 16
      where
        invs = zipWith centerI (concatMap row [i3,i3,i2,i2,i1]) places 
        [i1,i2,i3] = zip3 is es [30,20,10]
    row = replicate hcount
    places = [Point (margin+hsep*x) (margin+vsep*y+offset)|
                 y<-[5,4..1],x<-[0..hcount-1]]
    offset = invHeight*min 8 startLevel

    centerI (pm@(pm1,_),ex,score) p =
        Inv pm ex score (p + pP ((invWidth-width pm1) `div` 2) 0)
    hsep = invWidth+4
    vsep = invHeight+8
    dx0 = 2

--------------------------------------------------------------------------------

type Output = Either Bases (Maybe Score)
type Input = Either Tick Click
type Bases = Drawing () Pict

--getGs = toMs getG
--putLivesMs l = toMsc (putLives l)
--putScoreMs s = toMsc (putScore s)
--putGUIMs g = toMsc (putGUI g)
--puatsGUIMs gs = toMsc (putsGUI gs)

--objectsK :: Size -> World -> Mk (G Input Output) ()
objectsK space world0 = reactiveSP (objectsKs space world0) world0

--objectsKs :: Size -> World -> Ms (G Input Output) World noreturn
objectsKs space world0 = playK
  where
    playK = message low high
      where
        high = either (const tickK) (const newK)

        newK = do set world1
                  putGUI (clearRect (Rect 0 space) True)
                  putScore Nothing
                  b <- field base
                  putLives (spare (bPict b) (lives b-1))
          where
            world1 = world0{invaders=invs1}
            invs1 = invs0{invs=[],delay=100}
            invs0 = invaders world0

        low ev =
          case ev of
	    Redraw        -> draw =<< get
            Key Fire Down -> fireTorpedo
            Key a    p    -> modBase (accelerate p a)

    accelerate pressed button b@ExB{} = b
    accelerate pressed button b@B{bSpeed=v} = b{bSpeed=v'}
      where
	v'   = case pressed of Down -> bdir ; _ -> 0
	bdir = case button of
                  MoveLeft -> -dx
                  MoveRight -> dx
                  _ -> 0
      

    tickK =
      do tickInvadersK
         tickUfoK
         W g shelters base ufo torpedo vshots invaders <- get
         base' <- tickBaseK base
         (torpedo',invaders',shelters',ufo')<-
                tickTorpedoK torpedo invaders shelters ufo
         set (W g shelters' base' ufo' torpedo' vshots invaders')
         tickVShotsK vshots base'

    tickInvadersK =
      do state@W{invaders=inv} <- get
         let t = delay inv-1
         case t of
           0 -> do let invcnt = length (invs inv)
                       l = level inv+1
                       offset = pP 0 (invHeight*(min 8 l-startLevel))
                       inv' = if invcnt==0
                              then move offset (invaders world0){level=l}
                              else inv{delay=(invcnt+6) `div` 4}
                   set state{invaders=inv'}
                   moveInvadersK
                   update fireVShot
           _ -> do let inv' = inv{delay=t}
                   set state{invaders=inv'}

    --tickBaseK :: Base -> Ms (G hi (Either Bases x)) s Base
    tickBaseK b@(ExB pm ex p@(Point x y) l t) =
        case t of
          50 -> putGUI (clearpm p pm) >> return b'{bPos=Point (-width pm) y}
          95  | l>0 -> putLives (spare pm (l-2)) >> return b'
          100 | l>1 -> do let b' = B pm ex (baseStartPos space pm) (l-1) 0
                          draw b'
                          return b'
          _ -> return b'
      where
        b' = b{exbAge=t+1}
        new = B pm ex
    tickBaseK b@(B pm ex p@(Point x y) l v) =
      case v of
        0 -> return b
	_ -> putGUI (clearpm p pm) >> draw b' >> return b'
	  where b' = B pm ex p' l v
	        p' = Point x' y
                x' = min (xcoord space-margin-width pm) (max margin (x+v))


    tickUfoK =
      do state@W{rnd=g,ufo=u@(U pm gc s)} <- get
         let ufoWidth = xcoord (size pm)
         case s of
           Wait 0 -> do let u'=U pm gc (Moving score dx (pP x0 margin))
                            score = 50*n
                            (x0,dx) = if rev
                                      then (xcoord space,-ufoDx)
                                      else (-ufoWidth,ufoDx)
                            (n,g') = randomR (1,6) g
                            (rev,g'') = random g'
                        draw u'
                        set state{rnd=g'',ufo=u'}
           Wait t -> set state{ufo=U pm gc (Wait (t-1))}
           Hit 0 s p -> do putGUI (clearpm p pm)
                           set state{ufo=U pm gc (Wait ufoDelay)}
           Hit t s p -> set state{ufo=U pm gc (Hit (t-1) s p)}
           Moving s dx p ->
             if dx>0 && xcoord p>=xcoord space ||
                dx<0 && xcoord p< -ufoWidth
             then do putGUI (clearpm p pm)
                     set state{ufo=U pm gc (Wait ufoDelay)}
             else do let p' = p + pP dx 0
                         u' = U pm gc (Moving s dx p')
                     putGUI (clearpm p pm)
                     draw u'
                     set state{ufo=u'}

    tickTorpedoK t@(T pm optp) inv sh ufo = 
      case optp of
        Nothing -> return (t,inv,sh,ufo)
	Just p@(Point x y) ->
	  if y<torpedoDy
	  then putGUI (clearpm p t) >> return (T pm Nothing,inv,sh,ufo)
	  else case p `invHit` inv of
	         (inv1,Inv (p1,_) ex score ip:inv2) ->
		     do putsGUI [clear p torpedoSize, clearpm ip p1]
                        draw exv
                        putScore (Just score)
		        return (T pm Nothing,inv{invs=inv'},sh,ufo)
                   where
                     inv' = inv1++exv:inv2
                     exv = Exv ex ip
		 _ -> case shelterHit' sh r d of
                        Just sh' -> do putGUI (clearRect d False)
                                       return (T pm Nothing,inv,sh',ufo)
                        _ -> case Rect p (size pm) `ufoHit` ufo of
                               Just ufo'@(U upm _ (Hit _ score up)) ->
                                 do putGUI (clearpm p t)
                                    putGUI (clearpm up upm)
                                    draw ufo'
                                    putScore (Just score)
                                    return (T pm Nothing,inv,sh,ufo')
                               _ -> do let t' = move (pP 0 (-torpedoDy)) t
                                       putGUI (clearpm p t)
                                       draw t'
                                       return (t',inv,sh,ufo)
                    where s = size pm
                          r = Rect (p+pP 0 (torpedoDy `div` 2)) s
                          d = Rect (p-pP 1 0) (s+pP 2 0)

    r `ufoHit` U pm gc (Moving s dx p)
               | overlaps r (Rect p (size pm)) = Just (U pm gc (Hit 100 s p))
    r `ufoHit` _  = Nothing

    p `invHit` Invs{invs=is} = break isHit is
      where
        isHit Exv{} = False
        isHit (Inv (p1,_) _ _ ip) = p `inRect` r
	  where r = Rect (ip + Point 2 0) (size p1 - Point 4 0)

    fireTorpedo = do a <- field alive
                     when a $ do update fire; draw =<< field torpedo
    
    fire w@W{torpedo=T gc t} =
      case t of
        Nothing -> w{torpedo=T gc (Just (firePos (base w)))}
	_ -> w

    fireVShot w@W{vshots=V pm@(p1,_) ps,invaders=Invs{invs=is},rnd=g0} =
        if null is || length ps>5 
        then w
        else if r<9
             then w{rnd=g1}
             else w{vshots=V pm (p:ps),rnd=g'}
      where
        p = Point (x+(invWidth-width p1) `div` 2) (y+invHeight-6)
        y = maximum [ycoord p|Inv{invPos=p}<-is,abs (xcoord p-x)<h]
        h = invWidth `div` 2
        x = xs !! ix
        xs = nub [xcoord p|Inv{invPos=p}<-is]
        
        (ix,g') = randomR (0,length xs-1) g1
        (r,g1) = randomR (0,9::Int) g0
      
    tickVShotsK (V pm@(p1,_) ps) base =
        do ps0' <- mapM moveVShot ps
           w <- get
           set w{vshots=V pm [p|Just p<-ps0']}
      where
        br = Rect (bPos base) (size base)

        v = pP 0 vshotDy
        ss@(Point w h) = size p1

        moveVShot p =
            if hitBase
            then do putGUI (clearpm p p1)
                    let base' = exBase base
                    draw base'
                    modBase (const base')
                    return Nothing
            else do sh <- field shelters
                    case shelterHit' sh hr dr of
                      Just sh' -> do w<-get
                                     set w{shelters=sh'}
                                     putGUI (clearRect dr False)
                                   --draw sh'
                                     return Nothing
                      _ -> if ycoord p' < ycoord space
                           then do putsGUI [clearpm p p1,wDraw (V1 pm p')]
                                   return (Just p')
                           else do putGUI (clearpm p p1)
                                   return Nothing
          where
            p' = p+v
            sr = Rect p ss
            hr = move (pP 0 (vshotDy-h)) sr
            dr = sr
            hitBase = overlaps br sr

    moveInvadersK =
        do w@W{invaders=invs0,shelters=ss} <- get
           let (movecmds,br',invs') = moveInvaders invs0
           putsGUI (concat movecmds)
           ss' <- if any (overlaps br' . shBR) ss
                  then let shHit ss i@Inv{} = maybe ss id.shelterHit ss $ rect i
                           shHit ss _       = ss
                       in return $ foldl shHit ss (invs invs')
                  else return ss
           set w{invaders=invs',shelters=ss'}

    moveInvaders (Invs is l dx dt) = (movecmds,br,Invs is' l dx' dt')
      where
        dt' = if null is' then 200 else if off_bottom then -1 else dt
        (dx',d) = if dx>0 && off_right || dx<0 && off_left
	          then (-dx,pP 0 invDy)
		  else (dx,pP dx 0)
	(xs,ys) = unzip [(x,y)|Inv{invPos=Point x y}<-is]
	off_left = min_x < margin
	off_right = max_x > xcoord space-margin
        off_bottom = max_y > ycoord space-margin
        min_x = minimum xs
        min_y = minimum ys
        max_x = maximum xs + invWidth
        max_y = maximum ys + invHeight
        br = if null xs
             then Rect 0 0
             else move d $ line2rect (lL min_x min_y max_x max_y)

	(movecmds,is0') = unzip (map moveInvader is)
        is' = [i|Just i<-is0']

        moveInvader (Exv p1 p) = ([clearpm p p1],Nothing)
	moveInvader (Inv (p1,p2) ex s p) =
	    case ycoord d of
	      0 -> ([wDraw i'],Just i')
	      _ -> ([clearpm p p1,wDraw i'],Just i')
	  where
	    i' = Inv (p2,p1) ex s (p+d)

shelterHit ss r = shelterHit' ss r r

shelterHit' ss r d =
  case break (hitSh r) ss of
    (pps1,Sh gc br rs:pps2) ->
        if any (overlaps r) rs && rs' /= rs
        then if null rs'
             then Just (pps1++pps2)
             else Just (pps1++Sh gc br rs':pps2)
        else Nothing
      where
        rs' = concat [diffRect sr d|sr<-rs]
    _ -> Nothing

hitSh r (Sh _ br _) = overlaps r br

--------------------------------------------------------------------------------

draw obj = putGUI (wDraw obj)

instance Draw World where
  drawing w = drawing ((shelters w,(base w,torpedo w)),(vshots w,invaders w))

--instance Draw Shelters where
--  drawing (Sh pps) = pps

instance Draw Shelter where
  drawing (Sh gc (Rect p s) rs) = [pp (rectangles gc s rs) 0]

instance Draw Base where
  drawing (B pm ex p _ _) = [pp pm p]
  drawing (ExB pm ex p _ _) = [pp ex p]

instance Draw Torpedo where
  drawing (T _ Nothing) = []
  drawing (T pm (Just p)) = [pp pm p]

instance Draw Ufo where
  drawing (U pm _  (Moving s dx p)) = [pp pm p]
  drawing (U pm gc (Hit t s p)) = [pp (text gc (show s)) (p+pP 0 (height pm))]
  drawing _ = []

instance Draw Invaders where
  drawing = drawing . invs

instance Draw Invader where
  drawing (Inv (pm,_) _ _ p) = [pp pm p]
  drawing (Exv pm p) = [pp pm p]

instance Draw VShots where
  drawing (V pm ps) = drawing [V1 pm p|p<-ps]

instance Draw VShot where
  drawing (V1 (pm1,pm2) p) =  [pp pm p]
    where
      pm = if even (ycoord p `div` vshotDy) then pm1 else pm2

--------------------------------------------------------------------------------
class HasSize a => HasRect a where rect :: a -> Rect

width obj = xcoord (size obj)
height obj = ycoord (size obj)

instance HasSize Base where size = size . bPict
instance HasRect Base where rect (B pm _ p _ _) = Rect p (size pm)

instance HasSize Torpedo where size _ = torpedoSize

instance HasSize Invader where
  size inv@Inv{invPict=(p1,_)} = size p1
  size inv@Exv{exvPict=p1} = size p1

instance HasRect Invader where
  rect inv = Rect (invPos inv) (size inv)

instance Move Base     where move v (B pm ex p l dx) = B pm ex (p+v) l dx
instance Move Torpedo  where move v (T pm op) = T pm (move v op)
instance Move Invaders where move v (Invs is l dx dt) = Invs (move v is) l dx dt
instance Move Invader  where move v inv@Inv{invPos=p} = inv{invPos=move v p}
instance Move VShots   where move v (V pm ps) = V pm (move v ps)

--------------------------------------------------------------------------------
firePos base = bPos base + pP (width base `div` 2) (-torpedoHeight)

shelterSize = pP 40 32

torpedoSize = pP 1 torpedoHeight
torpedoHeight = 16
torpedoDy = 12

ufoDelay = argReadKey "ufodelay" 2500
ufoDx = 2

vshotDy = 4

invHeight = 16
invWidth = 24
invDy = 3*invHeight `div` 4

margin = 10
dx = argReadKey "dx" 3::Int -- base speed

hcount = argReadKey "hcount" 11::Int
startLevel = argReadKey "level" 0::Int
