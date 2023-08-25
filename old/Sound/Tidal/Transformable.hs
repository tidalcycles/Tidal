module Sound.Tidal.Transformable where

class Transformable f where
  rev :: f a -> f a
  cat :: [f a] -> f a
  ply :: f Rational -> f a -> f a
  _ply :: Rational ->f a-> f a
  stack :: [f a] -> f a
  euclid :: f Int -> f Int -> f String -> f String
  fast :: f Rational -> f a -> f a
  slow :: f Rational -> f a -> f a
  _euclid :: Int -> Int -> f a-> f a
  _slow :: Rational -> f a -> f a
  _fast :: Rational -> f a -> f a
  timeCat :: [(Rational, f a)] -> f a
  timecat :: [(Rational, f a)] -> f a
  fastAppend :: f a -> f a -> f a
  fastappend :: f a -> f a -> f a
  slowAppend :: f a -> f a -> f a
  slowappend :: f a -> f a -> f a
  append :: f a ->  f a -> f a
  fromList :: [a] -> f a
  fastFromList :: [a] -> f a
  fromMaybes :: [Maybe a] -> f a
  run :: (Enum a, Num a) => f a -> f a
  _run :: (Enum a, Num a) => a -> f a
  scan :: (Enum a, Num a) => f a -> f a
  _scan :: (Enum a, Num a) => a -> f a
  every :: f Int -> (f b -> f b) -> f b -> f b
  _every :: Int -> (f a -> f a) -> f a -> f a
  listToPat :: [a] -> f a
  fastcat :: [f a] -> f a
  fastCat :: [f a] -> f a
  slowcat :: [f a] -> f a
  slowCat :: [f a] -> f a
  density :: f Rational -> f a-> f a
  rotL :: Rational -> f a -> f a
  rotR :: Rational -> f a -> f a
  iter :: f Int -> f a -> f a
  iter' :: f Int -> f a -> f a
  _iter :: Int -> f a -> f a
  _iter' :: Int -> f a -> f a
  -- (<~) :: f Rational -> f a -> f a
  -- (~>) :: f Rational -> f a -> f a
