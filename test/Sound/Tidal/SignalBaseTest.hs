{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalBaseTest where

import           Test.Microspec              (MTestable (describe), Microspec,
                                              Testable (property), it, shouldBe,
                                              (===))
import           TestUtils                   (compareP, comparePD,
                                              stripMetadata, toEvent)

import           Prelude                     hiding ((*>), (<*))

import           Data.List                   (sort)
import           Data.Ratio                  ((%))

import           Sound.Tidal.Compose         (struct, (|+), (|=|))
import           Sound.Tidal.Params          (n, s)
import           Sound.Tidal.ParseBP         (parseBP_E)
import           Sound.Tidal.Pattern         (Pattern (atom, filterValues, rev, silence, stack, timeCat, (*>), (<*)),
                                              _slow, append, cat, early, euclid,
                                              euclidFull, euclidInv, every,
                                              fast, fastCat, late, ply, press,
                                              pressBy, range, run, slow,
                                              superimpose, (<~))
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Signal.Random   (irand)
import           Sound.Tidal.Signal.Waveform (saw, sine, tri)
import           Sound.Tidal.Types           (ArcF (Arc), Event (Event),
                                              Metadata (Metadata), Note,
                                              Signal (query), State (State),
                                              Time, Value (VF))

import qualified Data.Map.Strict             as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Signal.Base" $ do
    describe "atom" $ do
      it "fills a whole cycle" $ do
        property $ queryArc (atom 0) (Arc 0 1) === [(Event (Metadata []) (Just $ Arc 0 1) (Arc 0 1) (0 :: Int))]
      it "returns the active of an atom that you ask for, preserving the whole" $ do
        property $ queryArc (atom 0) (Arc 0.25 0.75) === [(Event (Metadata []) (Just $ Arc 0 1) (Arc 0.25 0.75) (0 :: Int))]
      it "gives correct fragments when you go over cycle boundaries" $ do
        property $ queryArc (atom 0) (Arc 0.25 1.25) ===
          [ (Event (Metadata []) (Just $ Arc 0 1) (Arc 0.25 1) (0 :: Int)),
            (Event (Metadata []) (Just $ Arc 1 2) (Arc 1 1.25) 0)
          ]
      it "works with zero-length queries" $ do
        it "0" $
          queryArc (atom "a") (Arc 0 0)
            `shouldBe` fmap toEvent [(((0,1), (0,0)), "a" :: String)]
        it "1/3" $
          queryArc (atom "a") (Arc (1%3) (1%3))
            `shouldBe` fmap toEvent [(((0,1), (1%3,1%3)), "a" :: String)]

    describe "_fastGap" $ do
      it "copes with cross-cycle queries" $ do
        (queryArc(_fastGap 2 $ fastCat [atom "a", atom "b"]) (Arc 0.5 1.5))
          `shouldBe`
          [(Event (Metadata []) (Just $ Arc (1 % 1) (5 % 4)) (Arc (1 % 1) (5 % 4)) ("a" :: String)),
           (Event (Metadata []) (Just $ Arc (5 % 4) (3 % 2)) (Arc (5 % 4) (3 % 2)) "b")
          ]
      it "copes with breaking up events across cycles" $ do
        (queryArc (stripMetadata $ _fastGap 2 $ slow 2 "a") (Arc 0 2))
          `shouldBe`
          [(Event (Metadata []) (Just $ Arc 0 1) (Arc 0 0.5) ("a" :: String)),
           (Event (Metadata []) (Just $ Arc 0.5 1.5) (Arc 1 1.5) "a")
          ]


      it "does not return events outside of the query" $ do
        (queryArc(_fastGap 2 $ fastCat [atom "a", atom ("b" :: String)]) (Arc 0.5 0.9))
          `shouldBe` []

    describe "<*>" $ do
      it "can apply a signal of values to a signal of values" $ do
        queryArc ((atom (+1)) <*> (atom 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
      it "can take structure from the left" $ do
        queryArc ((fastCat [atom (+1), atom (+2)]) <*> (atom 3)) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 4 :: Int),
           (((0.5,1), (0.5,1)), 5)
          ]
      it "can take structure from the right" $ do
        queryArc (atom (+1) <*> (fastCat [atom 7, atom 8])) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9)
          ]
      it "can take structure from the both sides" $ do
        it "one" $
          queryArc ((fastCat [atom (+1), atom (+2)]) <*> (fastCat [atom 7, atom 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 10)
          ]
        it "two" $
          queryArc ((fastCat [atom (+1), atom (+2), atom (+3)]) <*> (fastCat [atom 7, atom 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [ (((0%1, 1%3), (0%1, 1%3)), 8 :: Int),
            (((1%3, 1%2), (1%3, 1%2)), 9),
            (((1%2, 2%3), (1%2, 2%3)), 10),
            (((2%3, 1%1), (2%3, 1%1)), 11)
          ]
      it "obeys atom id <*> v = v" $ do
        let v = (fastCat [fastCat [atom 7, atom 8], atom 9]) :: Signal Int
        queryArc ((atom id <*> v)) (Arc 0 5) `shouldBe` queryArc v (Arc 0 5)

      it "obeys atom f <*> atom x = atom (f x)" $ do
        let f = (+3)
            x = 7 :: Int
        queryArc (atom f <*> atom x) (Arc 0 5) `shouldBe` queryArc (atom (f x)) (Arc 0 5)

      it "obeys u <*> atom y = atom ($ y) <*> u" $ do
        let u = fastCat [atom (+7), atom (+8)]
            y = 6 :: Int
        queryArc (u <*> atom y) (Arc 0 5) `shouldBe` queryArc (atom ($ y) <*> u) (Arc 0 5)

      it "obeys atom (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
        let u = (fastCat [atom (+7), atom (+8)]) :: Signal (Int -> Int)
            v = fastCat [atom (+3), atom (+4), atom (+5)]
            w = fastCat [atom 1, atom 2]
        queryArc (atom (.) <*> u <*> v <*> w) (Arc 0 5) `shouldBe` queryArc (u <*> (v <*> w)) (Arc 0 5)


    describe "<*" $ do
      it "can apply a signal of values to a signal of functions" $ do
        queryArc ((atom (+1)) <* (atom 3)) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,1), (0,1)), 4  :: Int)]
      it "doesn't take structure from the right" $ do
        queryArc (atom (+1) <* (fastCat [atom 7, atom 8])) (Arc 0 1)
          `shouldBe` fmap toEvent [(((0,1), (0,0.5)), 8 :: Int),
                                   (((0,1), (0.5,1)), 9 :: Int)
                                  ]

    describe "*>" $ do
      it "can apply a signal of values to a signal of functions" $ do
        it "works within cycles" $ queryArc ((atom (+1)) *> (atom 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
        it "works across cycles" $ queryArc ((atom (+1)) *> (slow 2 $ atom 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,2), (0,1)), 4  :: Int)]
      it "doesn't take structure from the left" $ do
        queryArc (atom (+1) *> (fastCat [atom 7, atom 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9 :: Int)
          ]

    describe "mixJoin" $ do
      it "preserves inner structure" $ do
        it "one" $
          (queryArc (mixJoin $ atom (fastCat [atom "a", atom ("b" :: String)])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [atom "a", atom "b"]) (Arc 0 1))
        it "two" $
          (queryArc (mixJoin $ atom (fastCat [atom "a", atom "b", fastCat [atom "c", atom ("d" :: String)]])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [atom "a", atom "b", fastCat [atom "c", atom "d"]]) (Arc 0 1))
      it "preserves outer structure" $ do
        it "one" $
          (queryArc (mixJoin $ fastCat [atom $ atom "a", atom $ atom ("b" :: String)]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [atom "a", atom "b"]) (Arc 0 1))
        it "two" $
          (queryArc (mixJoin $ fastCat [atom $ atom "a", atom $ atom "b", fastCat [atom $ atom "c", atom $ atom ("d" :: String)]]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [atom "a", atom "b", fastCat [atom "c", atom "d"]]) (Arc 0 1))
      it "gives events whole/active timespans that are an intersection of that of inner and outer events" $ do
        let a = fastCat [atom "a", atom "b"]
            b = fastCat [atom "c", atom "d", atom "e"]
            pp = fastCat [atom a, atom b]
        queryArc (mixJoin pp) (Arc 0 1)
          `shouldBe` [(Event (Metadata []) (Just $ Arc (0 % 1) (1 % 2)) (Arc (0 % 1) (1 % 2)) ("a" :: String)),
                      (Event (Metadata []) (Just $ Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "d"),
                      (Event (Metadata []) (Just $ Arc (2 % 3) (1 % 1)) (Arc (2 % 3) (1 % 1)) "e")
                     ]

    describe "squeezeJoin" $ do
      it "compresses cycles to fit outer 'whole' timearc of event" $ do
        let a = fastCat [atom "a", atom "b"]
            b = fastCat [atom "c", atom "d", atom "e"]
            pp = fastCat [atom a, atom b]
        queryArc (squeezeJoin pp) (Arc 0 1)
          `shouldBe` [(Event (Metadata []) (Just $ Arc (0 % 1) (1 % 4)) (Arc (0 % 1) (1 % 4)) ("a" :: String)),
                      (Event (Metadata []) (Just $ Arc (1 % 4) (1 % 2)) (Arc (1 % 4) (1 % 2)) "b"),
                      (Event (Metadata []) (Just $ Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "c"),
                      (Event (Metadata []) (Just $ Arc (2 % 3) (5 % 6)) (Arc (2 % 3) (5 % 6)) "d"),
                      (Event (Metadata []) (Just $ Arc (5 % 6) (1 % 1)) (Arc (5 % 6) (1 % 1)) "e")
                     ]

    describe ">>=" $ do
      it "can apply functions to signals" $ do
       let p = fastCat [atom 7, atom 8] :: Signal Int
           p' = do x <- p
                   return $ x + 1
       (queryArc p' (Arc 0 1)) `shouldBe` (queryArc ((+1) <$> p) (Arc 0 1))

      it "can add two signals together" $ do
       let p1 = fastCat [atom 7, atom 8, atom 9] :: Signal Int
           p2 = fastCat [atom 4, fastCat [atom 5, atom 6]]
           p' = do x <- p1
                   y <- p2
                   return $ x + y
       compareP (Arc 0 1) p' ((+) <$> p1 <*> p2)

      it "conforms to (return v) >>= f = f v" $ do
       let f x = atom $ x + 10
           v = 5 :: Int
       compareP (Arc 0 5) ((return v) >>= f) (f v)
      it "conforms to m >>= return ≡ m" $ do
       let m = fastCat [atom "a", fastCat [atom "b", atom ("c" :: String)]]
       compareP (Arc 0 1) (m >>= return) m
     --    it "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )" $ do
     --      let m = fastCat [atom "a", fastCat [atom "b", atom "c"]]

    describe "late" $ do
      it "works over two cycles" $
       property $ comparePD (Arc 0 2) (0.25 ~> atom "a") (0.25 `late` atom ("a" :: String))
      it "works over one cycle" $
       property $ compareP (Arc 0 1) (0.25 ~> atom "a") (0.25 `late` atom ("a" :: String))
      it "works with zero width queries" $
       property $ compareP (Arc 0 0) (0.25 ~> atom "a") (0.25 `late` atom ("a" :: String))

    -- This is now in TestUtils.hs
    describe "comparePD" $ do
      it "allows split events to be compared" $
       property $ comparePD (Arc 0 2)
         (splitQueries $ _slow 2 $ atom ("a" :: String))
         (_slow 2 $ atom "a")

    describe "cF_" $ do
      it "can retrieve values from state" $
       (query (atom 3 + cF_ "hello") $ State (Arc 0 1) (Map.singleton "hello" (VF 0.5)))
       `shouldBe` [(Event (Metadata []) (Just $ Arc (0 % 1) (1 % 1)) (Arc (0 % 1) (1 % 1)) 3.5)]

    describe "withEventArc" $ do
     it "apply given function to the Arcs" $ do
      let p = withEventArc (+5) (stripMetadata $ fast "1 2" "3 4" :: Signal Int)
      let res = queryArc p (Arc 0 1)
      property $ res === fmap toEvent [(((5, 11%2), (5, 11%2)), 3), (((11%2, 23%4), (11%2, 23%4)), 3), (((23%4, 6), (23%4, 6)), 4)]


    describe "filterValues" $ do
     it "remove Events above given threshold" $ do
       let fil = filterValues (<2) $ fastCat [atom 1, atom 2, atom 3] :: Signal Time
       let res = queryArc fil (Arc 0.5 1.5)
       property $ fmap toEvent [(((1, 4%3), (1, 4%3)), 1%1)] === res

     it "remove Events below given threshold" $ do
       let fil = filterValues (>2) $ fastCat [atom 1, atom 2, atom 3] :: Signal Time
       let res = queryArc fil (Arc 0.5 1.5)
       property $ fmap toEvent [(((2%3, 1), (2%3, 1)), 3%1)] === res

    describe "filterTime" $ do
      it "filter below given threshold" $ do
        let fil = filterTime (<0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        property $ [] === res

      it "filter above given threshold" $ do
        let fil = stripMetadata $ filterTime (>0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        property $ fmap toEvent [(((3%4, 1), (3%4, 1)), 1.25), (((1, 5%4), (1, 5%4)), 1.25), (((5%4, 3%2), (5%4, 3%2)), 1.75)] === res

    describe "_compressArc" $ do
      it "return empty if start time is greater than end time" $ do
        let res = queryArc (_compressArc (Arc 0.8 0.1) (fast "1 2" "3 4" :: Signal Time) ) (Arc 1 2)
        property $ [] === res

      it "return empty if start time or end time are greater than 1" $ do
        let res = queryArc (_compressArc (Arc 0.1 2) (fast "1 2" "3 4" :: Signal Time)) (Arc 1 2)
        property $ [] === res

      it "return empty if start or end are less than zero" $ do
        let res = queryArc (_compressArc (Arc (-0.8) 0.1) (fast "1 2" "3 4" :: Signal Time)) (Arc 1 2)
        property $ [] === res

      it "otherwise compress difference between start and end values of Arc" $ do
        let p = fast "1 2" "3 4" :: Signal Time
        let res = queryArc (stripMetadata $ _compressArc (Arc 0.2 0.8) p) (Arc 0 1)
        let expected = fmap toEvent [(((1%5, 1%2), (1%5, 1%2)), 3%1), (((1%2, 13%20), (1%2, 13%20)), 3%1), (((13%20, 4%5), (13%20, 4%5)), 4%1)]
        property $ expected === res

    describe "timecat" $ do
      it "works across cycle boundaries" $ do
        queryArc (timeCat [(1, (slow 2 "a") :: Signal String)]) (Arc 0 2)
        `shouldBe`
        queryArc (slow 2 "a" :: Signal String) (Arc 0 2)

    describe "every" $
      it "`every n id` doesn't change the signal's structure" $ do
        comparePD
          (Arc 0 4)
          (every 2 id "x/2" :: Signal String)
          "x/2"

    describe "loopFirst" $ do
      it "plays the first cycle" $ do
        compareP (Arc 0 1)
          (loopFirst $ early 3 $ slow 8 $ "0 .. 7" :: Signal Int)
          ("3")
      it "plays the first cycle" $ do
        compareP (Arc 0 1)
          (fast 4 $ loopFirst $ "<0 1 2 3>" :: Signal Int)
          ("0 0 0 0")

    describe "append" $
      it "can switch between the cycles from two pures" $ do
        queryArc (append (pure "a") (pure "b")) (Arc 0 5)
          `shouldBe` fmap
            toEvent
            [ (((0, 1), (0, 1)), "a" :: String),
              (((1, 2), (1, 2)), "b"),
              (((2, 3), (2, 3)), "a"),
              (((3, 4), (3, 4)), "b"),
              (((4, 5), (4, 5)), "a")
            ]

    describe "cat" $ do
      it "can switch between the cycles from three pures" $ do
        queryArc (cat [pure "a", pure "b", pure "c"]) (Arc 0 5)
          `shouldBe` fmap
            toEvent
            [ (((0, 1), (0, 1)), "a" :: String),
              (((1, 2), (1, 2)), "b"),
              (((2, 3), (2, 3)), "c"),
              (((3, 4), (3, 4)), "a"),
              (((4, 5), (4, 5)), "b")
            ]
      it "can extract nested revs" $
        let a = "1 2 3" :: Signal Int
            b = "4 5 6" :: Signal Int
            c = "7 8 9" :: Signal Int
         in comparePD
              (Arc 0 10)
              (rev $ cat [a, b, c])
              (cat [rev a, rev b, rev c])

    describe "fastCat" $ do
      it "can switch between the cycles from three pures inside one cycle" $ do
        it "1" $
          queryArc (fastCat [pure "a", pure "b", pure "c"]) (Arc 0 1)
            `shouldBe` fmap
              toEvent
              [ (((0, 1 / 3), (0, 1 / 3)), "a" :: String),
                (((1 / 3, 2 / 3), (1 / 3, 2 / 3)), "b"),
                (((2 / 3, 1), (2 / 3, 1)), "c")
              ]
        it "5/3" $
          queryArc (fastCat [pure "a", pure "b", pure "c"]) (Arc 0 (5 / 3))
            `shouldBe` fmap
              toEvent
              [ (((0, 1 / 3), (0, 1 / 3)), "a" :: String),
                (((1 / 3, 2 / 3), (1 / 3, 2 / 3)), "b"),
                (((2 / 3, 1), (2 / 3, 1)), "c"),
                (((1, 4 / 3), (1, 4 / 3)), "a"),
                (((4 / 3, 5 / 3), (4 / 3, 5 / 3)), "b")
              ]
      it "works with zero-length queries" $ do
        it "0" $
          queryArc (fastCat [pure "a", pure "b"]) (Arc 0 0)
            `shouldBe` fmap toEvent [(((0, 0.5), (0, 0)), "a" :: String)]
        it "1/3" $
          queryArc (fastCat [pure "a", pure "b"]) (Arc (1 % 3) (1 % 3))
            `shouldBe` fmap toEvent [(((0, 0.5), (1 % 3, 1 % 3)), "a" :: String)]

    describe "rev" $ do
      it "mirrors events" $ do
        let forward = fastCat [fastCat [pure 7, pure 8], pure 9] :: Signal Int
            backward = fastCat [pure 9, fastCat [pure 8, pure 7]]
        -- sort the events into time order to compare them
        sort (queryArc (rev forward) (Arc 0 1)) `shouldBe` sort (queryArc backward (Arc 0 1))

      it "returns the original if you reverse it twice" $ do
        let x = fastCat [fastCat [pure 7, pure 8], pure 9] :: Signal Int
        queryArc (rev $ rev x) (Arc 0 5) `shouldBe` queryArc x (Arc 0 5)


    describe "|=|" $ do
      let a = "[1, 1] [2,2] 3" :: Signal Int
          b = "4 [5, 5] 6 7" :: Signal Int
          c = "7 8 9 10" :: Signal Int
          d = "7 [8, 9] 10 11" :: Signal Int
      it "creates silence when" $ do
        it "first argument silent" $
          comparePD
            (Arc 0 1)
            (silence |=| a)
            silence
        it "second argument silent" $
          comparePD
            (Arc 0 1)
            (a |=| silence)
            silence
      it "creates the same signal when left argument has the same structure" $
        comparePD
          (Arc 0 1)
          (b |=| a)
          (d |=| a)
      it "can extract rev from first argument" $
        comparePD
          (Arc 0 1)
          (rev a |=| b)
          (rev (a |=| rev b))
      it "is assiociative" $
        comparePD
          (Arc 0 1)
          ((a |=| b) |=| c)
          (a |=| (b |=| c))
      it "is commutative in all arguments except the rightmost" $
        comparePD
          (Arc 0 1)
          (a |=| b |=| c)
          (b |=| a |=| c)

    describe "stack" $ do
      let a = "1 2 3" :: Signal Int
          b = "4 5 6" :: Signal Int
          c = "7 8 9" :: Signal Int
      it "is neutral with silence" $
        comparePD
          (Arc 0 1)
          (stack [a, silence])
          a
      it "can create silence" $
        comparePD
          (Arc 0 1)
          (stack [] :: Signal Int)
          silence
      it "follows commutative laws" $
        comparePD
          (Arc 0 1)
          (stack [a, b])
          (stack [b, a])
      it "follows associative laws" $
        comparePD
          (Arc 0 1)
          (stack [a, stack [b, c]])
          (stack [stack [a, b], c])
      it "can extract nested revs" $
        comparePD
          (Arc 0 1)
          (rev $ stack [a, b, c])
          (stack [rev a, rev b, rev c])


    describe "fast" $ do
      let x = "1 2 3" :: Signal Time
          y = "4 5 6" :: Signal Time
      it "is neutral with speedup 1" $
        comparePD
          (Arc 0 1)
          (fast 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Arc 0 1)
            (fast silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Arc 0 1)
            (fast x silence :: Signal Time)
            silence
        it "speedup by 0" $
          comparePD
            (Arc 0 1)
            (fast 0 x)
            silence
      it "is reciprocal to slow" $
        comparePD
          (Arc 0 1)
          (fast 2 x)
          (slow (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal speedup" $
        comparePD
          (Arc 0 1)
          (fast 2 $ fast (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Arc 0 1)
          (fast x (stack [y, y]))
          (fast (stack [x, x]) y)

    describe "slow" $ do
      let x = "1 2 3" :: Signal Time
          y = "4 5 6" :: Signal Time
      it "is neutral with slowdown 1" $
        comparePD
          (Arc 0 10)
          (slow 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Arc 0 10)
            (slow silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Arc 0 10)
            (slow x silence :: Signal Time)
            silence
        it "speedup by 0" $
          comparePD
            (Arc 0 10)
            (slow 0 x)
            silence
      it "is reciprocal to fast" $
        comparePD
          (Arc 0 10)
          (slow 2 x)
          (fast (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal slowdown" $
        comparePD
          (Arc 0 10)
          (slow 2 $ slow (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Arc 0 1)
          (slow x (stack [y, y]))
          (slow (stack [x, x]) y)

    describe "compress" $ do
      it "squashes cycles to the start of a cycle" $ do
        let p = compress 0 0.5 $ fastCat [pure 7, pure 8] :: Signal Int
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.25), (0, 0.25)), 7),
              (((0.25, 0.5), (0.25, 0.5)), 8)
            ]
      it "squashes cycles to the end of a cycle" $ do
        let p = compress 0.5 0.5 $ fastCat [pure 7, pure 8] :: Signal Int
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.5, 0.75), (0.5, 0.75)), 7 :: Int),
              (((0.75, 1), (0.75, 1)), 8)
            ]
      it "squashes cycles to the middle of a cycle" $ do
        let p = compress 0.25 0.5 $ fastCat [pure 7, pure 8]
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.25, 0.5), (0.25, 0.5)), 7 :: Int),
              (((0.5, 0.75), (0.5, 0.75)), 8)
            ]

-- test from old 'UI' module

    describe "segment" $ do
      it "can turn a single event into multiple events" $ do
        compareP (Arc 0 3)
          (segment 4 "x")
          ("x*4" :: Signal String)
      it "can turn a continuous pattern into multiple discrete events" $ do
        compareP (Arc 0 3)
          (segment 4 saw)
          ("0.125 0.375 0.625 0.875" :: Signal Double)
      it "can hold a value over multiple cycles" $ do
        comparePD (Arc 0 8)
          (segment 0.5 saw)
          (slow 2 "0" :: Signal Double)
      {-
      -- not sure what this is supposed to do!
      it "holding values over multiple cycles works in combination" $ do
        comparePD (Arc 0 8)
          ("0*4" |+ (_segment (1/8) $ saw))
          ("0*4" :: Signal Double)
      -}


    describe "range" $ do
      describe "scales a pattern to the supplied range" $ do
        describe "from 3 to 4" $ do
          it "at the start of a cycle" $
            (queryArc (range 3 4 saw) (Arc 0 0)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0 0) (3 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (range 3 4 saw) (Arc 0.25  0.25)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.25 0.25) (3.25 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (range 3 4 saw) (Arc 0.75 0.75)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.75 0.75) (3.75 :: Float)]

        describe "from -1 to 1" $ do
          it "at 1/2 of a cycle" $
            (queryArc (range (-1) 1 saw) (Arc 0.5 0.5)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.5 0.5) (0 :: Float)]

        describe "from 4 to 2" $ do
          it "at the start of a cycle" $
            (queryArc (range 4 2 saw) (Arc 0 0)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0 0) (4 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (range 4 2 saw) (Arc 0.25 0.25)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.25 0.25) (3.5 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (range 4 2 saw) (Arc 0.75 0.75)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.75 0.75) (2.5 :: Float)]

        describe "from 10 to 10" $ do
          it "at 1/2 of a cycle" $
            (queryArc (range 10 10 saw) (Arc 0.5 0.5)) `shouldBe`
              [Event (Metadata []) Nothing (Arc 0.5 0.5) (10 :: Float)]

    describe "rot" $ do
      it "rotates values in a pattern irrespective of structure" $
        property $ comparePD (Arc 0 2)
          (rot 1 "a ~ b c" :: Signal String)
          ( "b ~ c a" :: Signal String)
      it "works with negative values" $
        property $ comparePD (Arc 0 2)
          (rot (-1) "a ~ b c" :: Signal String)
          ( "c ~ a b" :: Signal String)
      it "works with complex patterns" $
        property $ comparePD (Arc 0 2)
          (rot (1) "a ~ [b [c ~ d]] [e <f g>]" :: Signal String)
          ( "b ~ [c [d ~ e]] [<f g> a]" :: Signal String)

    describe "ply" $ do
      it "can ply chords" $ do
        compareP (Arc 0 1)
          (ply 3 "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*3 6*3" :: Signal Int)
      it "can pattern the ply factor" $ do
        compareP (Arc 0 1)
          (ply "3 4 5" "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*4 6*5" :: Signal Int)

    describe "press" $ do
      it "can syncopate a pattern" $ do
        compareP (Arc 0 1)
          (press "a b [c d] e")
          ("[~ a] [~ b] [[~ c] [~ d]] [~ e]" :: Signal String)
    describe "pressBy" $ do
      it "can syncopate a pattern by a given amount" $ do
        compareP (Arc 0 1)
          (pressBy (1/3) "a b [~ c]")
          ("[~ a@2] [~ b@2] [~ [~ c@2]]" :: Signal String)



    describe "rolledBy" $ do
      it "shifts each start of events in a list correctly" $ do
        let
          overTimeSpan = (Arc 0 1)
          testMe = rolledBy "0.5" $ n ("[0,1,2,3]")
          expectedResult = n "[0, ~ 1@7, ~@2 2@6, ~@3 3@5]"
          in
            compareP overTimeSpan testMe expectedResult
      it "shifts each start of events in a list correctly in reverse order" $ do
        let
          overTimeSpan = (Arc 0 1)
          testMe = rolledBy "-0.5" $ n ("[0,1,2,3]")
          expectedResult = n "[3, ~ 2@7, ~@2 1@6, ~@3 0@5]"
          in
            compareP overTimeSpan testMe expectedResult
      it "trims the result pattern if it becomes larger than the original pattern" $ do
        let
          overTimeSpan = (Arc 0  1)
          testMe = rolledBy "1.5" $ n ("[0,1,2]")
          expectedResult = n "[0, ~ 1]"
          in
            compareP overTimeSpan testMe expectedResult
      it "does nothing for continous functions" $ do
        let
          overTimeSpan = (Arc 0  1)
          testMe = n (rolledBy "0.25" (irand 0) |+ "[0,12]")
          expectedResult = n (irand 0) |+ n "[0, 12]"
          in
            compareP overTimeSpan testMe expectedResult
      it "does nothing when passing zero as time value" $ do
        let
          overTimeSpan = (Arc 0  1)
          testMe = n (rolledBy "0" "[0,1,2,3]")
          expectedResult = n "[0,1,2,3]"
          in
            compareP overTimeSpan testMe expectedResult

    describe "euclid" $ do
      it "matches examples in Toussaint's paper" $ do
        sequence_ $ map (\(a,b) -> it b $ compareP (Arc 0 1) a (parseBP_E b))
          ([(euclid 1 2 "x", "x ~"),
            (euclid 1 3 "x", "x ~ ~"),
            (euclid 1 4 "x", "x ~ ~ ~"),
            (euclid 4 12 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~"),
            (euclid 2 5 "x", "x ~ x ~ ~"),
            -- (euclid 3 4 "x", "x ~ x x"), -- Toussaint is wrong..
            (euclid 3 4 "x", "x x x ~"), -- correction
            (euclid 3 5 "x", "x ~ x ~ x"),
            (euclid 3 7 "x", "x ~ x ~ x ~ ~"),
            (euclid 3 8 "x", "x ~ ~ x ~ ~ x ~"),
            (euclid 4 7 "x", "x ~ x ~ x ~ x"),
            (euclid 4 9 "x", "x ~ x ~ x ~ x ~ ~"),
            (euclid 4 11 "x", "x ~ ~ x ~ ~ x ~ ~ x ~"),
            -- (euclid 5 6 "x", "x ~ x x x x"), -- Toussaint is wrong..
            (euclid 5 6 "x", "x x x x x ~"),  -- correction
            (euclid 5 7 "x", "x ~ x x ~ x x"),
            (euclid 5 8 "x", "x ~ x x ~ x x ~"),
            (euclid 5 9 "x", "x ~ x ~ x ~ x ~ x"),
            (euclid 5 11 "x", "x ~ x ~ x ~ x ~ x ~ ~"),
            (euclid 5 12 "x", "x ~ ~ x ~ x ~ ~ x ~ x ~"),
            -- (euclid 5 16 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~ x ~ ~ ~ ~"),  -- Toussaint is wrong..
            (euclid 5 16 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~ x ~ ~ ~"), -- correction
            -- (euclid 7 8 "x", "x ~ x x x x x x"), -- Toussaint is wrong..
            (euclid 7 8 "x", "x x x x x x x ~"), -- Correction
            (euclid 7 12 "x", "x ~ x x ~ x ~ x x ~ x ~"),
            (euclid 7 16 "x", "x ~ ~ x ~ x ~ x ~ ~ x ~ x ~ x ~"),
            (euclid 9 16 "x", "x ~ x x ~ x ~ x ~ x x ~ x ~ x ~"),
            (euclid 11 24 "x", "x ~ ~ x ~ x ~ x ~ x ~ x ~ ~ x ~ x ~ x ~ x ~ x ~"),
            (euclid 13 24 "x", "x ~ x x ~ x ~ x ~ x ~ x ~ x x ~ x ~ x ~ x ~ x ~")
          ] :: [(Signal String, String)])
      it "can be called with a negative first value to give the inverse" $ do
        compareP (Arc 0 1)
          (euclid (-3) 8 ("bd" :: Signal String))
          (euclidInv 3 8 ("bd" :: Signal String))
      it "can be called with a negative first value to give the inverse (patternable)" $ do
        compareP (Arc 0 1)
          (euclid (-3) 8 ("bd" :: Signal String))
          ("bd(-3,8)" :: Signal String)

    describe "euclidFull" $ do
      it "can match against silence" $ do
        compareP (Arc 0 1)
          (euclidFull 3 8 "bd" silence)
          ("bd(3,8)" :: Signal String)

    describe "snowball" $ do
      let testSignal = ("1 2 3 4"::Signal Int)
      it "acummulates a transform version of a pattern and appends the result - addition" $ do
        compareP (Arc 0 1)
          (snowball 3 (+) (slow 2) (testSignal))
          (cat [testSignal,(testSignal+(slow 2 testSignal)),((testSignal+(slow 2 testSignal))+slow 2 (testSignal+(slow 2 testSignal)))])

    describe "soak" $ do
      it "applies a transform and then appends the result -- addition" $ do
        compareP (Arc 0 3)
          (soak 3 (+ 1) "4 ~ 0 1")
          (cat ["4 ~ 0 1"::Signal Int,"5 ~ 1 2"::Signal Int,"6 ~ 2 3"::Signal Int])
      it "applies a transform and then appends the result -- slow" $ do
        compareP (Arc 0 7)
          (soak 3 (slow 2) "4 ~ 0 1")
          (cat ["4 ~ 0 1"::Signal Int, slow 2 "4 ~ 0 1"::Signal Int, slow 4 "4 ~  0 1"::Signal Int])
      it "applies a transform and then appends the result -- addition patterns" $ do
        compareP (Arc 0 3)
          (soak 3 (+ "1 2 3") "1 1")
          (cat ["1 1"::Signal Int,"2 [3 3] 4"::Signal Int,"3 [5 5] 7"::Signal Int])

    describe "bite" $ do
      it "can slice a pattern into bits" $ do
        compareP (Arc 0 4)
          (bite 4 "0 2*2" (Sound.Tidal.Pattern.run 8))
          ("[0 1] [4 5]*2" :: Signal Int)
      it "can slice a pattern into patternable bits number" $ do
        compareP (Arc 0 4)
          (bite "8 4" "0 2*2" (Sound.Tidal.Pattern.run 8))
          ("[0] [4 5]*2" :: Signal Int)


    describe "chunk" $ do
      it "can chunk a rev pattern" $ do
        compareP (Arc 0 4)
          (chunk 2 (rev) $  ("a b c d" :: Signal String))
          (slow 2 $ "d c c d a b b a" :: Signal String)
      it "can chunk a fast pattern" $ do
        compareP (Arc 0 4)
          (chunk 2 (fast 2) $ "a b" :: Signal String)
          (slow 2 $ "a b b _ a _ a b" :: Signal String)
      it "should chunk backward with a negative number" $ do
        compareP (Arc 0 4)
          (chunk (-2) (rev) $ ("a b c d" :: Signal String))
          (slow 2 $ "a b b a d c c d" :: Signal String)

    describe "binary" $ do
      it "converts a number to a pattern of boolean" $ do
        compareP (Arc 0 1)
          (binary "128")
          ("t f f f f f f f" :: Signal Bool)

    describe "binaryN" $ do
      it "converts a number to a pattern of boolean of specified length" $ do
        compareP (Arc 0 1)
          (binaryN 4 "8")
          ("t f f f" :: Signal Bool)
      it "converts a number to a pattern of boolean of specified patternable length" $ do
        compareP (Arc 0 2)
          (binaryN "<4 8>" "8")
          (cat ["t f f f", "f f f f t f f f"] :: Signal Bool)

    describe "ascii" $ do
      it "converts characters to a pattern of bools" $ do
        compareP (Arc 0 1)
          (ascii "a b")
          ("f t t f f f f t f t t f f f t f" :: Signal Bool)

    describe "off" $ do
      it "superimposes and shifts pattern" $ do
        compareP (Arc 0 1)
          (off "-e" id $ s "0")
          (superimpose ("e" <~) $ s "0")


    describe "necklace" $ do
      it "can specify rhythm by IOI" $ do
        compareP (Arc 0 1)
          (necklace 12 [4,2])
          ("t f f f t f t f f f t f")

    describe "quantise" $ do
      it "can quantise notes" $ do
        compareP (Arc 0 1)
          (segment 2 $ quantise 1 $ sine :: Signal Note)
          ("1 0" :: Signal Note)


    describe "loopFirst" $ do
      it "plays the first n cycles" $ do
        compareP (Arc 0 1)
          (loopFirst $ early 3 $ slow 8 $ "0 .. 7" :: Signal Int)
          ("3")

    describe "loopCycles" $ do
      it "can loop time" $ do
        compareP (Arc 0 1)
          ((3 <~) $ (loopCycles 3 $ s "<a b c d>"))
          (s "a")
      it "can pattern time" $ do
        compareP (Arc 0 1)
          (fast 4 $ loopCycles "<2 2 1 1>" $ s "<a b c d>")
          (s "a b a a")

{-

    describe "arpeggiate" $ do
      it "can arpeggiate" $ do
         compareP (Arc 0 1)
           (arpeggiate ("[bd, sn] [hh:1, cp]" :: Signal String))
           ("bd sn hh:1 cp" :: Signal String)
      it "can arpeggiate" $ do
        compareP (Arc 0 4)
          (arpeggiate $ "[0,0] [0,0]")
          ("0 0 0 0" :: Signal Int)
      it "can arpeggiate a 'sped up' pattern" $ do
        compareP (Arc 0 4)
          (arpeggiate $ "[0,0]*2")
          ("0 0 0 0" :: Signal Int)

-}
