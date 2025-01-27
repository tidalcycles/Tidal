{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.StreamTest where

import qualified Data.Map.Strict as M
import qualified Sound.Osc.Fd as O
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Test.Microspec

run :: Microspec ()
run =
  describe "Sound.Tidal.Stream" $ do
    describe "toDatum" $ do
      it "should convert VN to osc float" $ do
        toDatum (VN (Note 3.5)) `shouldBe` O.float (3.5 :: Double)

    describe "substitutePath" $ do
      -- ValueMap
      let state = M.fromList [("sound", VS "sn"), ("n", VI 8)]
      it "should return same string if no params are specified" $ do
        substitutePath "/s_new" state `shouldBe` Just "/s_new"
      it "should substitute values for params if present" $ do
        substitutePath "/{sound}/{n}/vol" state `shouldBe` Just "/sn/8/vol"
      it "should return Nothing if a param is not present" $ do
        substitutePath "/{sound}/{inst}" state `shouldBe` Nothing

    describe "getString" $ do
      it "should return Nothing for missing params" $ do
        getString M.empty "s" `shouldBe` Nothing
      it "should work for strings" $ do
        getString (M.singleton "s" (VS "sn")) "s" `shouldBe` Just "sn"
      it "should work for params with fallback expressions" $ do
        getString (M.singleton "s" (VS "sn")) "s=bd" `shouldBe` Just "sn"
      it "should work for missing params with fallback expressions" $ do
        getString M.empty "s=bd" `shouldBe` Just "bd"
