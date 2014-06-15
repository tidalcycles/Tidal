{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Sound.Tidal.Strategies where

import Data.Ratio
import Control.Applicative

import Sound.Tidal.Dirt
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Time
import Sound.Tidal.Utils

stutter n t p = stack $ map (\i -> (t * (fromIntegral i)) ~> p) [0 .. (n-1)]

echo   = stutter 2
triple = stutter 3
quad   = stutter 4
double = echo

jux f p = stack [p |+| pan (pure 0), f $ p |+| pan (pure 1)]
juxcut f p = stack [p     |+| pan (pure 0) |+| cut (pure (-1)), 
                    f $ p |+| pan (pure 1) |+| cut (pure (-2))
                   ]
jux4 f p = stack [p |+| pan (pure 0), f $ p |+| pan (pure 2)]

superimpose f p = stack [p, f p]

-- every 4 (smash 4 [1, 2, 3]) $ sound "[odx sn/2 [~ odx] sn/3, [~ hh]*4]"

smash n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = striate n p

-- samples "jvbass [~ latibro] [jvbass [latibro jvbass]]" ((1%2) <~ slow 6 "[1 6 8 7 3]")

samples :: Applicative f => f String -> f Int -> f String
samples p p' = pick <$> p <*> p'

spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
spread f xs p = cat $ map (\x -> f x p) xs

slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
slowspread f xs p = slowcat $ map (\x -> f x p) xs

spread' :: (a -> Pattern b -> Pattern c) -> Pattern a -> Pattern b -> Pattern c
spread' f timepat pat =
  Pattern $ \r -> concatMap (\(_,r', x) -> (arc (f x pat) r')) (rs r)
  where rs r = arc (filterOnsetsInRange timepat) r

{-
scrumple :: Time -> Pattern a -> Pattern a -> Pattern a
scrumple o p p' = p'' -- overlay p (o ~> p'')
  where p'' = Pattern $ \a -> concatMap 
                              (\((s,d), vs) -> map (\x -> ((s,d),
                                                           snd x
                                                          )
                                                   )
                                                   (arc p' (s,s))
                              ) (arc p a)
-}

whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmod a b = Sound.Tidal.Pattern.when ((\t -> (t `mod` a) >= b ))

--rev :: Pattern a -> Pattern a
--rev p = Pattern $ \a -> concatMap 
--                        (\a' -> mapFsts mirrorArc $ 
--                                (arc p (mirrorArc a')))
--                        (arcCycles a)

trunc :: Time -> Pattern a -> Pattern a
trunc t p = slow t $ Pattern $ \a -> concatMap f $ arcCycles a
  where f a = mapArcs (stretch . trunc') $ arc p (trunc' a)
        trunc' (s,e) = (min s ((sam s) + t), min e ((sam s) + t))
        stretch (s,e) = (sam s + ((s - sam s) / t), sam s + ((e - sam s) / t))

--spreadf :: [Pattern a -> Pattern b] -> Pattern a -> Pattern b
spreadf ts p = spread ($)

spin :: Int -> OscPattern -> OscPattern
spin steps p = stack $ map (\n -> (((fromIntegral n)%(fromIntegral steps)) <~ p |+| pan (pure $ (fromIntegral n)/(fromIntegral steps)))) [0 .. steps]

{-stripe :: Arc -> Pattern a -> Pattern a
stripe (stripeS, stripeE) p = slow t $ Pattern $ \a -> concatMap f $ arcCycles a
  where f a = mapFsts (stretch . stripe') $ arc p (stripe' a)
        trunc' (s,e) = (min s ((sam s) + t), min e ((sam s) + t))
        stretch (s,e) = (sam s + ((s - sam s) / t), sam s + ((e - sam s) / t))
-}

spin4 step p = stack $ map (\n -> ((toRational n)/4) <~ p |+| pan (pure $ n)) [0,step .. 3]

spin16 step p = stack $ map (\n -> ((toRational n)/16) <~ p |+| pan (pure $ n)) [0,step .. 15]

sawwave4 = ((*4) <$> sawwave1)
sinewave4 = ((*4) <$> sinewave1)
rand4 = ((*4) <$> rand)

stackwith p ps | null ps = silence
               | otherwise = stack $ map (\(i, p') -> p' |+| (((fromIntegral i) % l) <~ p)) (zip [0 ..] ps)
  where l = fromIntegral $ length ps

{-
cross f p p' = Pattern $ \t -> concat [filter flt $ arc p t,
                                       filter (not . flt) $ arc p' t
                                      ]
  where flt = f . cyclePos . fst . fst
-}

inside n f p = density n $ f (slow n p)

stut :: Integer -> Double -> Rational -> OscPattern -> OscPattern
stut steps feedback time p = stack (p:(map (\x -> (((x%steps)*time) ~> (p |+| gain (pure $ scale (fromIntegral x))))) [0..(steps-1)])) 
  where scale x 
          = ((+feedback) . (*(1-feedback)) . (/(fromIntegral steps)) . ((fromIntegral steps)-)) x
