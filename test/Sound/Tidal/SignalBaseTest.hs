{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalBaseTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Pattern (atom, fastCat, slow, _slow, fast)
import           Sound.Tidal.Signal.Compose (struct)

import qualified Data.Map.Strict     as Map

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
