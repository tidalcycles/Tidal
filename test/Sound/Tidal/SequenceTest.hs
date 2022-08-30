{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import Sound.Tidal.Sequence

import Test.Microspec 
import Data.Ratio

import Prelude hiding ((*>), (<*))

run :: Microspec() 
run =
    describe "Sound.Tidal.Sequence" $ do
        describe "unwrapping a sequence" $
          it "can unwrap sequences" $ unwrap (Sequence [Atom 1 2, Sequence [Atom 1 3, Gap 1]]) ==  (Sequence [Atom 1 2, Atom 1 3, Gap 1])
        describe "plying a sequence" $ 
          it "can ply sequences of rationals" $ do
            unwrap (Sequence [Atom 1 2, Sequence [Atom 1 3, Gap 1]])
            `shouldBe` (Sequence [Atom 1 2, Atom 1 3, Gap 1])
        describe "Stratapply" $ do
          describe "JustifyLeft" $ 
            it "can justify sequence to left" $  do
              stratApply JustifyRight [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]]
              `shouldBe` Stack [Sequence [Atom (1 % 1) 3,Gap (4 % 1)],Sequence [Gap (2 % 1),Gap (3 % 1)],Sequence [Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7],Gap (0 % 1)]]
          describe "JustifyRight" $   
            it "can justify sequence to right" $ do 
              stratApply JustifyLeft [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]] 
              `shouldBe` Stack [Sequence [Atom (1 % 1) 3,Gap (4 % 1)],Sequence [Gap (2 % 1),Gap (3 % 1)],Sequence [Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7],Gap (0 % 1)]]
          describe "Centre" $
            it "can centre sequences" $ do 
              stratApply Centre [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]] 
              `shouldBe` Stack [Sequence [Gap (2 % 1),Atom (1 % 1) 3,Gap (2 % 1)],Sequence [Gap (3 % 2),Gap (2 % 1),Gap (3 % 2)],Sequence [Gap (0 % 1),Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7],Gap (0 % 1)]]
          describe "Expand" $ 
            it "can expand sequences" $ do 
              stratApply Expand [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]] 
              `shouldBe` Stack [Atom (5 % 1) 3,Gap (5 % 1),Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7]]
          describe "Squeeze" $   
            it "can squeeze sequences" $ do 
              stratApply Squeeze [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]] 
              `shouldBe` Stack [Atom (1 % 1) 3,Gap (1 % 1),Sequence [Atom (1 % 5) 2,Atom (4 % 5) 7]]
          describe "JustifyBoth" $ 
            it "can justifyBoth sequences" $ do 
              stratApply JustifyBoth [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]] 
              `shouldBe` Stack [Atom (5 % 1) 3,Gap (5 % 1),Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7]]
          describe "TruncateMin" $ 
            it "can truncateMin sequences" $ do 
              stratApply TruncateMin [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]]
              `shouldBe` Stack [Atom (1 % 1) 3,Gap (1 % 1),Sequence [Atom (1 % 1) 2]]
          describe "TruncateMax" $ 
            it "can truncateMax sequences" $ do
              stratApply TruncateMax [Atom 1 3, Gap 2, Sequence [Atom 1 2, Atom 4 7]]
              `shouldBe` Stack [Sequence [Atom (1 % 1) 3,Atom (1 % 1) 3,Atom (1 % 1) 3,Atom (1 % 1) 3,Atom (1 % 1) 3],Sequence [Gap (2 % 1),Gap (2 % 1),Gap (1 % 1)],Sequence [Atom (1 % 1) 2,Atom (4 % 1) 7]]
        describe "applying functions to sequences" $  do
          describe "Fast" $
            it "can speed up sequences" $ do 
              fast (Sequence [Atom 3 2, Gap 1]) (Sequence [Atom 4 5, Gap 3])  
              `shouldBe` Sequence [Atom (2 % 1) 5,Gap (5 % 8),Gap (7 % 4)]
          describe "slow" $ 
            it "can slow down sequences" $ do
              slow (Sequence [Atom 3 2, Gap 1]) (Sequence [Atom 4 5, Gap 3])
              `shouldBe` Sequence [Atom (8 % 1) 5,Gap (5 % 2),Gap (7 % 4)]
          describe "rep" $ 
            it "can replicate sequences" $ do 
              rep 4 (Sequence [Atom 1 4, Gap 2])
              `shouldBe` Sequence [Atom (1 % 1) 4,Gap (2 % 1),Atom (1 % 1) 4,Gap (2 % 1),Atom (1 % 1) 4,Gap (2 % 1),Atom (1 % 1) 4,Gap (2 % 1)]
          describe "repSqueeze" $ 
            it "can repSqueeze sequences" $ do 
              repSqueeze 4 (Sequence [Atom 1 4, Gap 2])
              `shouldBe` Sequence [Atom (1 % 4) 4,Gap (1 % 2),Atom (1 % 4) 4,Gap (1 % 2),Atom (1 % 4) 4,Gap (1 % 2),Atom (1 % 4) 4,Gap (1 % 2)]       
          describe "stack" $ 
            it "can stack sequences together" $ do 
              stack [Atom 1 4, Sequence [Atom 1 3, Gap 2], Gap 3]  
              `shouldBe` Stack [Atom (3 % 1) 4,Sequence [Atom (1 % 1) 3,Gap (2 % 1)],Gap (3 % 1)]
          describe "euclid" $ 
            it "can obtain a euclidean sequence" $ do 
              euclid (Atom 1 3) (Atom 1 8) (Atom 1 "bd")
              `shouldBe` Sequence [Atom (1 % 1) "bd",Gap (1 % 1),Gap (1 % 1),Atom (1 % 1) "bd",Gap (1 % 1),Gap (1 % 1),Atom (1 % 1) "bd",Gap (1 % 1)] 
          describe "every" $ 
            it "can apply every function" $  do 
              every (Sequence [Atom 1 3, Gap 2]) (fast (Atom 1 3)) (Sequence [Atom 1 "bd", Gap 2])
              `shouldBe` Sequence [Sequence [Atom (1 % 1) "bd",Gap (2 % 1),Atom (1 % 1) "bd",Gap (2 % 1),Atom (1 % 3) "bd",Gap (2 % 3)],Sequence [Atom (1 % 1) "bd",Gap (2 % 1)]]
          describe "fromList" $ 
            it "can obtain seqeunce from list" $ do 
              fromList [2,3,4,2,3] 
              `shouldBe` Sequence [Atom (1 % 1) 2,Atom (1 % 1) 3,Atom (1 % 1) 4,Atom (1 % 1) 2,Atom (1 % 1) 3]
          describe "fastFromList" $ 
            it "can obtain a squeezed sequence from a list" $ do 
              fastFromList [2,3,4,2,3]
              `shouldBe` Sequence [Atom (1 % 5) 2,Atom (1 % 5) 3,Atom (1 % 5) 4,Atom (1 % 5) 2,Atom (1 % 5) 3]
          describe "fastCat" $ 
            it "can concatenate and speed up a sequence" $ do 
              fastCat [Atom 1 2, Atom 1 5, Atom 2 3, Gap 4]
              `shouldBe` Sequence [Atom (1 % 8) 2,Atom (1 % 8) 5,Atom (1 % 4) 3,Gap (1 % 2)]
          describe "run" $ 
            it "can obtain increasing sequence" $ do  
              Sound.Tidal.Sequence.run (Atom 3 4)
              `shouldBe` Sequence [Atom (3 % 4) 0,Atom (3 % 4) 1,Atom (3 % 4) 2,Atom (3 % 4) 3]
          describe "scan" $ 
            it "can obtain a sequence of sequences of increasing sizes" $ do 
              scan (Atom 3 4)
              `shouldBe` Sequence [Atom (3 % 1) 0,Atom (3 % 2) 0,Atom (3 % 2) 1,Atom (1 % 1) 0,Atom (1 % 1) 1,Atom (1 % 1) 2,Atom (3 % 4) 0,Atom (3 % 4) 1,Atom (3 % 4) 2,Atom (3 % 4) 3]
          describe "iter" $ 
            it "can obtain an iterative sequence" $ do 
              iter (Atom 1 3) (Sequence [Atom 2 "bd", Gap 3])
              `shouldBe ` Sequence [Atom (2 % 1) "bd",Gap (3 % 1),Atom (5 % 3) "bd",Gap (3 % 1),Atom (1 % 3) "bd",Atom (4 % 3) "bd",Gap (3 % 1),Atom (2 % 3) "bd"]
          describe "iter'" $ 
            it "can perform reverse iter" $ do
              iter' (Atom 1 3) (Sequence [Atom 2 "bd", Gap 3])
              `shouldBe` Sequence [Atom (2 % 1) "bd",Gap (3 % 1),Gap (1 % 3),Atom (2 % 1) "bd",Gap (8 % 3),Gap (2 % 3),Atom (2 % 1) "bd",Gap (7 % 3)] 

        describe "applying functiions to sequences with strategies" $ do
          describe "fastS" $ 
            it "can speed up sequences" $ do 
              fastS (Sequence [Atom 3 2, Gap 1]) (Sequence [Atom 4 5, Gap 3]) JustifyLeft
              `shouldBe` Sequence [Atom (3 % 2) 5,Gap (1 % 1),Gap (3 % 1)]
          describe "slowS" $ 
            it "can slow down sequences" $ do 
              slowS (Sequence [Atom 3 2, Gap 1]) (Sequence [Atom 4 5, Gap 3]) JustifyRight
              `shouldBe` Sequence [Gap (3 % 1),Atom (2 % 1) 5,Gap (4 % 1),Gap (1 % 1)]
          describe "stackS" $ 
            it "can stack sequences" $ do 
              stackS [Atom 1 4, Sequence [Atom 1 3, Gap 2], Gap 3] Centre
              `shouldBe` Stack [Sequence [Gap (1 % 1),Atom (1 % 1) 4,Gap (1 % 1)],Sequence [Gap (0 % 1),Sequence [Atom (1 % 1) 3,Gap (2 % 1)],Gap (0 % 1)],Sequence [Gap (0 % 1),Gap (3 % 1),Gap (0 % 1)]]
          describe "everyS" $ 
            it "can apply every to a sequence" $  do 
              everyS (Sequence [Atom 1 3, Gap 2]) (fast (Atom 1 3)) (Sequence [Atom 3 "bd", Gap 2]) Expand
              `shouldBe` Sequence [Atom (3 % 1) "bd",Gap (2 % 1),Atom (3 % 1) "bd",Gap (2 % 1),Atom (1 % 1) "bd",Gap (2 % 3),Atom (3 % 1) "bd",Gap (2 % 1)]
          describe "iterS" $ 
            it "can perform iter with a strategy" $ do 
              iterS (Atom 1 3) (Sequence [Atom 2 "bd", Gap 3]) TruncateMin
              `shouldBe` Sequence [Atom (1 % 1) "bd",Atom (2 % 3) "bd",Atom (1 % 3) "bd",Atom (1 % 3) "bd",Atom (2 % 3) "bd"]
          describe "iterS'" $   
            it "can perform iter' with a strategy" $ do 
              iterS' (Atom 1 3) (Sequence [Atom 2 "bd", Gap 3]) JustifyBoth
              `shouldBe` Sequence [Atom (2 % 1) "bd",Gap (3 % 1),Gap (1 % 3),Atom (2 % 1) "bd",Gap (8 % 3),Gap (2 % 3),Atom (2 % 1) "bd",Gap (7 % 3)] 




          
