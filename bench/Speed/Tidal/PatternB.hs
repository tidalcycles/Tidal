module Tidal.PatternB where 

import           Criterion.Main    (Benchmark, bench, whnf)
import           Sound.Tidal.Arc   (hull, sect)
import           Sound.Tidal.Types (ArcF (Arc))

arc1 = Arc 3 5
arc2 = Arc 4 6
arc3 = Arc 0 1
arc4 = Arc 1 2

sectB :: Benchmark
sectB = bench "sect" $ whnf (sect arc1) arc2

hullB :: Benchmark 
hullB = bench "hull" $ whnf (hull arc1) arc2
