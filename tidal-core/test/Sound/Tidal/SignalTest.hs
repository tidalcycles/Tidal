{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalTest where

import           Sound.Tidal.TestUtils       (compareP, comparePD, toEvent)
import           Test.Microspec              (MTestable (describe), Microspec,
                                              Testable (property), it, shouldBe,
                                              (===))

import           Prelude                     hiding ((*>), (<*))

import           Data.List                   (sort)
import           Data.Ratio                  ((%))

import           Sound.Tidal.Compose         (struct, (|+), (|=|))
import           Sound.Tidal.InstanceHacks
import           Sound.Tidal.Mininotation    (parseBP_E)
import           Sound.Tidal.Params
import           Sound.Tidal.Pattern         (_slow, append, ascii, binary,
                                              binaryN, bite, chunk, early,
                                              every, fast, fastcat, late,
                                              necklace, ply, press, pressBy,
                                              range, run, segment, slow,
                                              snowball, soak, stripMetadata,
                                              (*>), (<*), (<~), (~>))
import           Sound.Tidal.Signal
import           Sound.Tidal.Signal.Input    (cF_)
import           Sound.Tidal.Signal.Random   (irand)
import           Sound.Tidal.Signal.Waveform (saw, tri)
import           Sound.Tidal.Types

import qualified Data.Map.Strict             as Map


s :: Pattern p => p String -> p ValueMap
s = pS "s"

n :: Pattern p => p Note -> p ValueMap
n = pN "n"

run :: Microspec ()
run =
  describe "Sound.Tidal.Signal.Base" $ do
    describe "pure" $ do
      it "fills a whole cycle" $ do
        property $ querySpan (pure 0) (Span 0 1) === [(Event mempty (Just $ Span 0 1) (Span 0 1) (0 :: Int))]
      it "returns the active part, preserving the whole" $ do
        property $ querySpan (pure 0) (Span 0.25 0.75) === [(Event mempty (Just $ Span 0 1) (Span 0.25 0.75) (0 :: Int))]
      it "gives correct fragments when you go over cycle boundaries" $ do
        property $ querySpan (pure 0) (Span 0.25 1.25) ===
          [ (Event mempty (Just $ Span 0 1) (Span 0.25 1) (0 :: Int)),
            (Event mempty (Just $ Span 1 2) (Span 1 1.25) 0)
          ]
      it "works with zero-length queries" $ do
        it "0" $
          querySpan (pure "a") (Span 0 0)
            `shouldBe` fmap toEvent [(((0,1), (0,0)), "a" :: String)]
        it "1/3" $
          querySpan (pure "a") (Span (1%3) (1%3))
            `shouldBe` fmap toEvent [(((0,1), (1%3,1%3)), "a" :: String)]

    describe "_fastGap" $ do
      it "copes with cross-cycle queries" $ do
        (querySpan(_fastGap 2 $ fastcat [pure "a", pure "b"]) (Span 0.5 1.5))
          `shouldBe`
          [(Event mempty (Just $ Span (1 % 1) (5 % 4)) (Span (1 % 1) (5 % 4)) ("a" :: String)),
           (Event mempty (Just $ Span (5 % 4) (3 % 2)) (Span (5 % 4) (3 % 2)) "b")
          ]
      it "copes with breaking up events across cycles" $ do
        (querySpan (stripMetadata $ _fastGap 2 $ slow 2 "a") (Span 0 2))
          `shouldBe`
          [(Event mempty (Just $ Span 0 1) (Span 0 0.5) ("a" :: String)),
           (Event mempty (Just $ Span 0.5 1.5) (Span 1 1.5) "a")
          ]


      it "does not return events outside of the query" $ do
        (querySpan(_fastGap 2 $ fastcat [pure "a", pure ("b" :: String)]) (Span 0.5 0.9))
          `shouldBe` []

    describe "<*>" $ do
      it "can apply a signal of values to a signal of values" $ do
        querySpan ((pure (+1)) <*> (pure 3)) (Span 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
      it "can take structure from the left" $ do
        querySpan ((fastcat [pure (+1), pure (+2)]) <*> (pure 3)) (Span 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 4 :: Int),
           (((0.5,1), (0.5,1)), 5)
          ]
      it "can take structure from the right" $ do
        querySpan (pure (+1) <*> (fastcat [pure 7, pure 8])) (Span 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9)
          ]
      it "can take structure from the both sides" $ do
        it "one" $
          querySpan ((fastcat [pure (+1), pure (+2)]) <*> (fastcat [pure 7, pure 8])) (Span 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 10)
          ]
        it "two" $
          querySpan ((fastcat [pure (+1), pure (+2), pure (+3)]) <*> (fastcat [pure 7, pure 8])) (Span 0 1)
          `shouldBe` fmap toEvent
          [ (((0%1, 1%3), (0%1, 1%3)), 8 :: Int),
            (((1%3, 1%2), (1%3, 1%2)), 9),
            (((1%2, 2%3), (1%2, 2%3)), 10),
            (((2%3, 1%1), (2%3, 1%1)), 11)
          ]
      it "obeys pure id <*> v = v" $ do
        let v = (fastcat [fastcat [pure 7, pure 8], pure 9]) :: Signal Int
        querySpan ((pure id <*> v)) (Span 0 5) `shouldBe` querySpan v (Span 0 5)

      it "obeys pure f <*> pure x = pure (f x)" $ do
        let f = (+3)
            x = 7 :: Int
        querySpan (pure f <*> pure x) (Span 0 5) `shouldBe` querySpan (pure (f x)) (Span 0 5)

      it "obeys u <*> pure y = pure ($ y) <*> u" $ do
        let u = fastcat [pure (+7), pure (+8)]
            y = 6 :: Int
        querySpan (u <*> pure y) (Span 0 5) `shouldBe` querySpan (pure ($ y) <*> u) (Span 0 5)

      it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
        let u = (fastcat [pure (+7), pure (+8)]) :: Signal (Int -> Int)
            v = fastcat [pure (+3), pure (+4), pure (+5)]
            w = fastcat [pure 1, pure 2]
        querySpan (pure (.) <*> u <*> v <*> w) (Span 0 5) `shouldBe` querySpan (u <*> (v <*> w)) (Span 0 5)


    describe "<*" $ do
      it "can apply a signal of values to a signal of functions" $ do
        querySpan ((pure (+1)) <* (pure 3)) (Span 0 1) `shouldBe` fmap toEvent
          [(((0,1), (0,1)), 4  :: Int)]
      it "doesn't take structure from the right" $ do
        querySpan (pure (+1) <* (fastcat [pure 7, pure 8])) (Span 0 1)
          `shouldBe` fmap toEvent [(((0,1), (0,0.5)), 8 :: Int),
                                   (((0,1), (0.5,1)), 9 :: Int)
                                  ]

    describe "*>" $ do
      it "can apply a signal of values to a signal of functions" $ do
        it "works within cycles" $ querySpan ((pure (+1)) *> (pure 3)) (Span 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
        it "works across cycles" $ querySpan ((pure (+1)) *> (slow 2 $ pure 3)) (Span 0 1) `shouldBe` fmap toEvent [(((0,2), (0,1)), 4  :: Int)]
      it "doesn't take structure from the left" $ do
        querySpan (pure (+1) *> (fastcat [pure 7, pure 8])) (Span 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9 :: Int)
          ]

    describe "mixJoin" $ do
      it "preserves inner structure" $ do
        it "one" $
          (querySpan (mixJoin $ pure (fastcat [pure "a", pure ("b" :: String)])) (Span 0 1))
          `shouldBe` (querySpan (fastcat [pure "a", pure "b"]) (Span 0 1))
        it "two" $
          (querySpan (mixJoin $ pure (fastcat [pure "a", pure "b", fastcat [pure "c", pure ("d" :: String)]])) (Span 0 1))
          `shouldBe` (querySpan (fastcat [pure "a", pure "b", fastcat [pure "c", pure "d"]]) (Span 0 1))
      it "preserves outer structure" $ do
        it "one" $
          (querySpan (mixJoin $ fastcat [pure $ pure "a", pure $ pure ("b" :: String)]) (Span 0 1))
          `shouldBe` (querySpan (fastcat [pure "a", pure "b"]) (Span 0 1))
        it "two" $
          (querySpan (mixJoin $ fastcat [pure $ pure "a", pure $ pure "b", fastcat [pure $ pure "c", pure $ pure ("d" :: String)]]) (Span 0 1))
          `shouldBe` (querySpan (fastcat [pure "a", pure "b", fastcat [pure "c", pure "d"]]) (Span 0 1))
      it "gives events whole/active timespans that are an intersection of that of inner and outer events" $ do
        let a = fastcat [pure "a", pure "b"]
            b = fastcat [pure "c", pure "d", pure "e"]
            pp = fastcat [pure a, pure b]
        querySpan (mixJoin pp) (Span 0 1)
          `shouldBe` [(Event mempty (Just $ Span (0 % 1) (1 % 2)) (Span (0 % 1) (1 % 2)) ("a" :: String)),
                      (Event mempty (Just $ Span (1 % 2) (2 % 3)) (Span (1 % 2) (2 % 3)) "d"),
                      (Event mempty (Just $ Span (2 % 3) (1 % 1)) (Span (2 % 3) (1 % 1)) "e")
                     ]

    describe "squeezeJoin" $ do
      it "compresses cycles to fit outer 'whole' timespan of event" $ do
        let a = fastcat [pure "a", pure "b"]
            b = fastcat [pure "c", pure "d", pure "e"]
            pp = fastcat [pure a, pure b]
        querySpan (squeezeJoin pp) (Span 0 1)
          `shouldBe` [(Event mempty (Just $ Span (0 % 1) (1 % 4)) (Span (0 % 1) (1 % 4)) ("a" :: String)),
                      (Event mempty (Just $ Span (1 % 4) (1 % 2)) (Span (1 % 4) (1 % 2)) "b"),
                      (Event mempty (Just $ Span (1 % 2) (2 % 3)) (Span (1 % 2) (2 % 3)) "c"),
                      (Event mempty (Just $ Span (2 % 3) (5 % 6)) (Span (2 % 3) (5 % 6)) "d"),
                      (Event mempty (Just $ Span (5 % 6) (1 % 1)) (Span (5 % 6) (1 % 1)) "e")
                     ]

    describe ">>=" $ do
      it "can apply functions to signals" $ do
       let p = fastcat [pure 7, pure 8] :: Signal Int
           p' = do x <- p
                   return $ x + 1
       (querySpan p' (Span 0 1)) `shouldBe` (querySpan ((+1) <$> p) (Span 0 1))

      it "can add two signals together" $ do
       let p1 = fastcat [pure 7, pure 8, pure 9] :: Signal Int
           p2 = fastcat [pure 4, fastcat [pure 5, pure 6]]
           p' = do x <- p1
                   y <- p2
                   return $ x + y
       compareP (Span 0 1) p' ((+) <$> p1 <*> p2)

      it "conforms to (return v) >>= f = f v" $ do
       let f x = pure $ x + 10
           v = 5 :: Int
       compareP (Span 0 5) ((return v) >>= f) (f v)
      it "conforms to m >>= return ≡ m" $ do
       let m = fastcat [pure "a", fastcat [pure "b", pure ("c" :: String)]]
       compareP (Span 0 1) (m >>= return) m
     --    it "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )" $ do
     --      let m = fastcat [pure "a", fastcat [pure "b", pure "c"]]

    describe "late" $ do
      it "works over two cycles" $
       property $ comparePD (Span 0 2) (0.25 ~> pure "a") (0.25 `late` pure ("a" :: String))
      it "works over one cycle" $
       property $ compareP (Span 0 1) (0.25 ~> pure "a") (0.25 `late` pure ("a" :: String))
      it "works with zero width queries" $
       property $ compareP (Span 0 0) (0.25 ~> pure "a") (0.25 `late` pure ("a" :: String))

    -- This is now in TestUtils.hs
    describe "comparePD" $ do
      it "allows split events to be compared" $
       property $ comparePD (Span 0 2)
         (splitQueries $ _slow 2 $ pure ("a" :: String))
         (_slow 2 $ pure "a")

    describe "cF_" $ do
      it "can retrieve values from state" $
       (query (pure 3 + cF_ "hello") $ State (Span 0 1) (Map.singleton "hello" (VF 0.5)))
       `shouldBe` [(Event mempty (Just $ Span (0 % 1) (1 % 1)) (Span (0 % 1) (1 % 1)) 3.5)]

    describe "filterValues" $ do
     it "remove Events above given threshold" $ do
       let fil = filterValues (<2) $ fastcat [pure 1, pure 2, pure 3] :: Signal Time
       let res = querySpan fil (Span 0.5 1.5)
       property $ fmap toEvent [(((1, 4%3), (1, 4%3)), 1%1)] === res

     it "remove Events below given threshold" $ do
       let fil = filterValues (>2) $ fastcat [pure 1, pure 2, pure 3] :: Signal Time
       let res = querySpan fil (Span 0.5 1.5)
       property $ fmap toEvent [(((2%3, 1), (2%3, 1)), 3%1)] === res

    describe "filterTime" $ do
      it "filter below given threshold" $ do
        let fil = filterTime (<0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = querySpan fil (Span 0.5 1.5)
        property $ [] === res

      it "filter above given threshold" $ do
        let fil = stripMetadata $ filterTime (>0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = querySpan fil (Span 0.5 1.5)
        property $ fmap toEvent [(((3%4, 1), (3%4, 1)), 1.25), (((1, 5%4), (1, 5%4)), 1.25), (((5%4, 3%2), (5%4, 3%2)), 1.75)] === res

    describe "_compressSpan" $ do
      it "return empty if start time is greater than end time" $ do
        let res = querySpan (_compressSpan (Span 0.8 0.1) (fast "1 2" "3 4" :: Signal Time) ) (Span 1 2)
        property $ [] === res

      it "return empty if start time or end time are greater than 1" $ do
        let res = querySpan (_compressSpan (Span 0.1 2) (fast "1 2" "3 4" :: Signal Time)) (Span 1 2)
        property $ [] === res

      it "return empty if start or end are less than zero" $ do
        let res = querySpan (_compressSpan (Span (-0.8) 0.1) (fast "1 2" "3 4" :: Signal Time)) (Span 1 2)
        property $ [] === res

      it "otherwise compress difference between start and end values of Span" $ do
        let p = fast "1 2" "3 4" :: Signal Time
        let res = querySpan (stripMetadata $ _compressSpan (Span 0.2 0.8) p) (Span 0 1)
        let expected = fmap toEvent [(((1%5, 1%2), (1%5, 1%2)), 3%1), (((1%2, 13%20), (1%2, 13%20)), 3%1), (((13%20, 4%5), (13%20, 4%5)), 4%1)]
        property $ expected === res

    describe "timecat" $ do
      it "works across cycle boundaries" $ do
        querySpan (timeCat [(1, (slow 2 "a") :: Signal String)]) (Span 0 2)
        `shouldBe`
        querySpan (slow 2 "a" :: Signal String) (Span 0 2)

    describe "every" $
      it "`every n id` doesn't change the signal's structure" $ do
        comparePD
          (Span 0 4)
          (every 2 id "x/2" :: Signal String)
          "x/2"

    describe "loopFirst" $ do
      it "plays the first cycle" $ do
        compareP (Span 0 1)
          (loopFirst $ early 3 $ slow 8 $ "0 .. 7" :: Signal Int)
          ("3")
      it "plays the first cycle" $ do
        compareP (Span 0 1)
          (fast 4 $ loopFirst $ "<0 1 2 3>" :: Signal Int)
          ("0 0 0 0")

    describe "append" $
      it "can switch between the cycles from two pures" $ do
        querySpan (append (pure "a") (pure "b")) (Span 0 5)
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
        querySpan (cat [pure "a", pure "b", pure "c"]) (Span 0 5)
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
              (Span 0 10)
              (rev $ cat [a, b, c])
              (cat [rev a, rev b, rev c])

    describe "fastcat" $ do
      it "can switch between the cycles from three pures inside one cycle" $ do
        it "1" $
          querySpan (fastcat [pure "a", pure "b", pure "c"]) (Span 0 1)
            `shouldBe` fmap
              toEvent
              [ (((0, 1 / 3), (0, 1 / 3)), "a" :: String),
                (((1 / 3, 2 / 3), (1 / 3, 2 / 3)), "b"),
                (((2 / 3, 1), (2 / 3, 1)), "c")
              ]
        it "5/3" $
          querySpan (fastcat [pure "a", pure "b", pure "c"]) (Span 0 (5 / 3))
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
          querySpan (fastcat [pure "a", pure "b"]) (Span 0 0)
            `shouldBe` fmap toEvent [(((0, 0.5), (0, 0)), "a" :: String)]
        it "1/3" $
          querySpan (fastcat [pure "a", pure "b"]) (Span (1 % 3) (1 % 3))
            `shouldBe` fmap toEvent [(((0, 0.5), (1 % 3, 1 % 3)), "a" :: String)]

    describe "rev" $ do
      it "mirrors events" $ do
        let forward = fastcat [fastcat [pure 7, pure 8], pure 9] :: Signal Int
            backward = fastcat [pure 9, fastcat [pure 8, pure 7]]
        -- sort the events into time order to compare them
        sort (querySpan (rev forward) (Span 0 1)) `shouldBe` sort (querySpan backward (Span 0 1))

      it "returns the original if you reverse it twice" $ do
        let x = fastcat [fastcat [pure 7, pure 8], pure 9] :: Signal Int
        querySpan (rev $ rev x) (Span 0 5) `shouldBe` querySpan x (Span 0 5)


    describe "|=|" $ do
      let a = "[1, 1] [2,2] 3" :: Signal Int
          b = "4 [5, 5] 6 7" :: Signal Int
          c = "7 8 9 10" :: Signal Int
          d = "7 [8, 9] 10 11" :: Signal Int
      it "creates silence when" $ do
        it "first argument silent" $
          comparePD
            (Span 0 1)
            (silence |=| a)
            silence
        it "second argument silent" $
          comparePD
            (Span 0 1)
            (a |=| silence)
            silence
      it "creates the same signal when left argument has the same structure" $
        comparePD
          (Span 0 1)
          (b |=| a)
          (d |=| a)
      it "can extract rev from first argument" $
        comparePD
          (Span 0 1)
          (rev a |=| b)
          (rev (a |=| rev b))
      it "is assiociative" $
        comparePD
          (Span 0 1)
          ((a |=| b) |=| c)
          (a |=| (b |=| c))
      it "is commutative in all arguments except the rightmost" $
        comparePD
          (Span 0 1)
          (a |=| b |=| c)
          (b |=| a |=| c)

    describe "stack" $ do
      let a = "1 2 3" :: Signal Int
          b = "4 5 6" :: Signal Int
          c = "7 8 9" :: Signal Int
      it "is neutral with silence" $
        comparePD
          (Span 0 1)
          (stack [a, silence])
          a
      it "can create silence" $
        comparePD
          (Span 0 1)
          (stack [] :: Signal Int)
          silence
      it "follows commutative laws" $
        comparePD
          (Span 0 1)
          (stack [a, b])
          (stack [b, a])
      it "follows associative laws" $
        comparePD
          (Span 0 1)
          (stack [a, stack [b, c]])
          (stack [stack [a, b], c])
      it "can extract nested revs" $
        comparePD
          (Span 0 1)
          (rev $ stack [a, b, c])
          (stack [rev a, rev b, rev c])


    describe "fast" $ do
      let x = "1 2 3" :: Signal Time
          y = "4 5 6" :: Signal Time
      it "is neutral with speedup 1" $
        comparePD
          (Span 0 1)
          (fast 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Span 0 1)
            (fast silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Span 0 1)
            (fast x silence :: Signal Time)
            silence
        it "speedup by 0" $
          comparePD
            (Span 0 1)
            (fast 0 x)
            silence
      it "is reciprocal to slow" $
        comparePD
          (Span 0 1)
          (fast 2 x)
          (slow (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal speedup" $
        comparePD
          (Span 0 1)
          (fast 2 $ fast (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Span 0 1)
          (fast x (stack [y, y]))
          (fast (stack [x, x]) y)

    describe "slow" $ do
      let x = "1 2 3" :: Signal Time
          y = "4 5 6" :: Signal Time
      it "is neutral with slowdown 1" $
        comparePD
          (Span 0 10)
          (slow 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Span 0 10)
            (slow silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Span 0 10)
            (slow x silence :: Signal Time)
            silence
        it "speedup by 0" $
          comparePD
            (Span 0 10)
            (slow 0 x)
            silence
      it "is reciprocal to fast" $
        comparePD
          (Span 0 10)
          (slow 2 x)
          (fast (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal slowdown" $
        comparePD
          (Span 0 10)
          (slow 2 $ slow (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Span 0 1)
          (slow x (stack [y, y]))
          (slow (stack [x, x]) y)

    describe "compress" $ do
      it "squashes cycles to the start of a cycle" $ do
        let p = compress 0 0.5 $ fastcat [pure 7, pure 8] :: Signal Int
        querySpan p (Span 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.25), (0, 0.25)), 7),
              (((0.25, 0.5), (0.25, 0.5)), 8)
            ]
      it "squashes cycles to the end of a cycle" $ do
        let p = compress 0.5 0.5 $ fastcat [pure 7, pure 8] :: Signal Int
        querySpan p (Span 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.5, 0.75), (0.5, 0.75)), 7 :: Int),
              (((0.75, 1), (0.75, 1)), 8)
            ]
      it "squashes cycles to the middle of a cycle" $ do
        let p = compress 0.25 0.5 $ fastcat [pure 7, pure 8]
        querySpan p (Span 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.25, 0.5), (0.25, 0.5)), 7 :: Int),
              (((0.5, 0.75), (0.5, 0.75)), 8)
            ]

-- test from old 'UI' module

    describe "segment" $ do
      it "can turn a single event into multiple events" $ do
        compareP (Span 0 3)
          (segment 4 "x")
          ("x*4" :: Signal String)
      it "can turn a continuous pattern into multiple discrete events" $ do
        compareP (Span 0 3)
          (segment 4 saw)
          ("0.125 0.375 0.625 0.875" :: Signal Double)
      it "can hold a value over multiple cycles" $ do
        comparePD (Span 0 8)
          (segment 0.5 saw)
          (slow 2 "0" :: Signal Double)
      {-
      -- not sure what this is supposed to do!
      it "holding values over multiple cycles works in combination" $ do
        comparePD (Span 0 8)
          ("0*4" |+ (_segment (1/8) $ saw))
          ("0*4" :: Signal Double)
      -}


    describe "range" $ do
      describe "scales a pattern to the supplied range" $ do
        describe "from 3 to 4" $ do
          it "at the start of a cycle" $
            (querySpan (range 3 4 saw) (Span 0 0)) `shouldBe`
              [Event mempty Nothing (Span 0 0) (3 :: Float)]
          it "at 1/4 of a cycle" $
            (querySpan (range 3 4 saw) (Span 0.25  0.25)) `shouldBe`
              [Event mempty Nothing (Span 0.25 0.25) (3.25 :: Float)]
          it "at 3/4 of a cycle" $
            (querySpan (range 3 4 saw) (Span 0.75 0.75)) `shouldBe`
              [Event mempty Nothing (Span 0.75 0.75) (3.75 :: Float)]

        describe "from -1 to 1" $ do
          it "at 1/2 of a cycle" $
            (querySpan (range (-1) 1 saw) (Span 0.5 0.5)) `shouldBe`
              [Event mempty Nothing (Span 0.5 0.5) (0 :: Float)]

        describe "from 4 to 2" $ do
          it "at the start of a cycle" $
            (querySpan (range 4 2 saw) (Span 0 0)) `shouldBe`
              [Event mempty Nothing (Span 0 0) (4 :: Float)]
          it "at 1/4 of a cycle" $
            (querySpan (range 4 2 saw) (Span 0.25 0.25)) `shouldBe`
              [Event mempty Nothing (Span 0.25 0.25) (3.5 :: Float)]
          it "at 3/4 of a cycle" $
            (querySpan (range 4 2 saw) (Span 0.75 0.75)) `shouldBe`
              [Event mempty Nothing (Span 0.75 0.75) (2.5 :: Float)]

        describe "from 10 to 10" $ do
          it "at 1/2 of a cycle" $
            (querySpan (range 10 10 saw) (Span 0.5 0.5)) `shouldBe`
              [Event mempty Nothing (Span 0.5 0.5) (10 :: Float)]

    describe "rot" $ do
      it "rotates values in a pattern irrespective of structure" $
        property $ comparePD (Span 0 2)
          (rot 1 "a ~ b c" :: Signal String)
          ( "b ~ c a" :: Signal String)
      it "works with negative values" $
        property $ comparePD (Span 0 2)
          (rot (-1) "a ~ b c" :: Signal String)
          ( "c ~ a b" :: Signal String)
      it "works with complex patterns" $
        property $ comparePD (Span 0 2)
          (rot (1) "a ~ [b [c ~ d]] [e <f g>]" :: Signal String)
          ( "b ~ [c [d ~ e]] [<f g> a]" :: Signal String)

    describe "ply" $ do
      it "can ply chords" $ do
        compareP (Span 0 1)
          (ply 3 "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*3 6*3" :: Signal Int)
      it "can pattern the ply factor" $ do
        compareP (Span 0 1)
          (ply "3 4 5" "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*4 6*5" :: Signal Int)

    describe "press" $ do
      it "can syncopate a pattern" $ do
        compareP (Span 0 1)
          (press "a b [c d] e")
          ("[~ a] [~ b] [[~ c] [~ d]] [~ e]" :: Signal String)
    describe "pressBy" $ do
      it "can syncopate a pattern by a given amount" $ do
        compareP (Span 0 1)
          (pressBy (1/3) "a b [~ c]")
          ("[~ a@2] [~ b@2] [~ [~ c@2]]" :: Signal String)



    describe "rolledBy" $ do
      it "shifts each start of events in a list correctly" $ do
        let
          overTimeSpan = (Span 0 1)
          testMe = rolledBy "0.5" $ n ("[0,1,2,3]")
          expectedResult = n "[0, ~ 1@7, ~@2 2@6, ~@3 3@5]"
          in
            compareP overTimeSpan testMe expectedResult
      it "shifts each start of events in a list correctly in reverse order" $ do
        let
          overTimeSpan = (Span 0 1)
          testMe = rolledBy "-0.5" $ n ("[0,1,2,3]")
          expectedResult = n "[3, ~ 2@7, ~@2 1@6, ~@3 0@5]"
          in
            compareP overTimeSpan testMe expectedResult
      it "trims the result pattern if it becomes larger than the original pattern" $ do
        let
          overTimeSpan = (Span 0  1)
          testMe = rolledBy "1.5" $ n ("[0,1,2]")
          expectedResult = n "[0, ~ 1]"
          in
            compareP overTimeSpan testMe expectedResult
      it "does nothing for continous functions" $ do
        let
          overTimeSpan = (Span 0  1)
          testMe = n (rolledBy "0.25" (irand 0) |+ "[0,12]")
          expectedResult = n (irand 0) |+ n "[0, 12]"
          in
            compareP overTimeSpan testMe expectedResult
      it "does nothing when passing zero as time value" $ do
        let
          overTimeSpan = (Span 0  1)
          testMe = n (rolledBy "0" "[0,1,2,3]")
          expectedResult = n "[0,1,2,3]"
          in
            compareP overTimeSpan testMe expectedResult

    describe "snowball" $ do
      let testSignal = ("1 2 3 4"::Signal Int)
      it "acummulates a transform version of a pattern and appends the result - addition" $ do
        compareP (Span 0 1)
          (snowball 3 (+) (slow 2) (testSignal))
          (cat [testSignal,(testSignal+(slow 2 testSignal)),((testSignal+(slow 2 testSignal))+slow 2 (testSignal+(slow 2 testSignal)))])

    describe "soak" $ do
      it "applies a transform and then appends the result -- addition" $ do
        compareP (Span 0 3)
          (soak 3 (+ 1) "4 ~ 0 1")
          (cat ["4 ~ 0 1"::Signal Int,"5 ~ 1 2"::Signal Int,"6 ~ 2 3"::Signal Int])
      it "applies a transform and then appends the result -- slow" $ do
        compareP (Span 0 7)
          (soak 3 (slow 2) "4 ~ 0 1")
          (cat ["4 ~ 0 1"::Signal Int, slow 2 "4 ~ 0 1"::Signal Int, slow 4 "4 ~  0 1"::Signal Int])
      it "applies a transform and then appends the result -- addition patterns" $ do
        compareP (Span 0 3)
          (soak 3 (+ "1 2 3") "1 1")
          (cat ["1 1"::Signal Int,"2 [3 3] 4"::Signal Int,"3 [5 5] 7"::Signal Int])

    describe "bite" $ do
      it "can slice a pattern into bits" $ do
        compareP (Span 0 4)
          (bite 4 "0 2*2" (Sound.Tidal.Pattern.run 8))
          ("[0 1] [4 5]*2" :: Signal Int)
      it "can slice a pattern into patternable bits number" $ do
        compareP (Span 0 4)
          (bite "8 4" "0 2*2" (Sound.Tidal.Pattern.run 8))
          ("[0] [4 5]*2" :: Signal Int)


    describe "chunk" $ do
      it "can chunk a rev pattern" $ do
        compareP (Span 0 4)
          (chunk 2 (rev) $  ("a b c d" :: Signal String))
          (slow 2 $ "d c c d a b b a" :: Signal String)
      it "can chunk a fast pattern" $ do
        compareP (Span 0 4)
          (chunk 2 (fast 2) $ "a b" :: Signal String)
          (slow 2 $ "a b b _ a _ a b" :: Signal String)
      it "should chunk backward with a negative number" $ do
        compareP (Span 0 4)
          (chunk (-2) (rev) $ ("a b c d" :: Signal String))
          (slow 2 $ "a b b a d c c d" :: Signal String)

    describe "binary" $ do
      it "converts a number to a pattern of boolean" $ do
        compareP (Span 0 1)
          (binary "128")
          ("t f f f f f f f" :: Signal Bool)

    describe "binaryN" $ do
      it "converts a number to a pattern of boolean of specified length" $ do
        compareP (Span 0 1)
          (binaryN 4 "8")
          ("t f f f" :: Signal Bool)
      it "converts a number to a pattern of boolean of specified patternable length" $ do
        compareP (Span 0 2)
          (binaryN "<4 8>" "8")
          (cat ["t f f f", "f f f f t f f f"] :: Signal Bool)

    describe "ascii" $ do
      it "converts characters to a pattern of bools" $ do
        compareP (Span 0 1)
          (ascii "a b")
          ("f t t f f f f t f t t f f f t f" :: Signal Bool)

    describe "necklace" $ do
      it "can specify rhythm by IOI" $ do
        compareP (Span 0 1)
          (necklace 12 [4,2])
          ("t f f f t f t f f f t f")

    describe "loopFirst" $ do
      it "plays the first n cycles" $ do
        compareP (Span 0 1)
          (loopFirst $ early 3 $ slow 8 $ "0 .. 7" :: Signal Int)
          ("3")

    describe "loopCycles" $ do
      it "can loop time" $ do
        compareP (Span 0 1)
          ((3 <~) $ (loopCycles 3 $ s "<a b c d>"))
          (s "a")
      it "can pattern time" $ do
        compareP (Span 0 1)
          (fast 4 $ loopCycles "<2 2 1 1>" $ s "<a b c d>")
          (s "a b a a")

{-

    describe "arpeggiate" $ do
      it "can arpeggiate" $ do
         compareP (Span 0 1)
           (arpeggiate ("[bd, sn] [hh:1, cp]" :: Signal String))
           ("bd sn hh:1 cp" :: Signal String)
      it "can arpeggiate" $ do
        compareP (Span 0 4)
          (arpeggiate $ "[0,0] [0,0]")
          ("0 0 0 0" :: Signal Int)
      it "can arpeggiate a 'sped up' pattern" $ do
        compareP (Span 0 4)
          (arpeggiate $ "[0,0]*2")
          ("0 0 0 0" :: Signal Int)

-}
