module Sound.Tidal.Common where

import Sound.Tidal.Pattern as Pat
import Sound.Tidal.Core as Pat
-- import Sound.Tidal.UI as Pat

import Sound.Tidal.Sequence as Seq

class Transformable f where
  rev :: f a -> f a
  cat :: [f a] -> f a

instance Transformable Pattern where
  rev = Pat.rev
  cat = Pat.cat

instance Transformable Branch where
  rev = Seq.rev
  cat = Seq.cat

seqPat :: Seq.Branch a -> Pat.Pattern a
seqPat (Seq.Atom _ a) = pure a
seqPat (Seq.Silence _) = Pat.silence
seqPat (Seq.Sequence bs) = Pat.timecat $ map (\b -> (seqSpan b, seqPat b)) bs
seqPat (Seq.Stack Expand bs) = Pat.stack $ map seqPat bs
seqPat b@(Seq.Stack JustifyLeft bs) =
  Pat.stack $ map (\b' -> _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs
seqPat b@(Seq.Stack JustifyRight bs) =
  Pat.stack $
    map (\b' -> rotR (1- (1/(seqSpan b / seqSpan b'))) $ _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs
seqPat b@(Seq.Stack Centre bs) = Pat.stack $
    map (\b' -> rotR (1.5/(seqSpan b / seqSpan b')) $ _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs

{-
data Strategy = JustifyBoth
              | Expand
              | TruncateMax
              | TruncateMin
              | RepeatLCM
-}
