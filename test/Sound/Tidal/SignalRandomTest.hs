
    describe "sometimesBy" $ do
      it "does nothing when set at 0% probability" $ do
        let
          overTimeSpan = (Arc 0  1)
          testMe = sometimesBy 0 (rev) (ps "bd*2 hh sn")
          expectedResult = ps "bd*2 hh sn"
          in
            compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function when set at 100% probability" $ do
        let
          overTimeSpan = (Arc 0  1)
          testMe = sometimesBy 1 (rev) (ps "bd*2 hh cp")
          expectedResult = ps "cp hh bd*2"
          in
            compareP overTimeSpan testMe expectedResult

        describe "sometimesBy'" $ do
      it "does nothing when set at 0% probability -- using const" $ do
        let
          overTimeSpan = (Arc 0  2)
          testMe = sometimesBy' 0 (const $ s "cp") (s "bd*8")
          expectedResult = s "bd*8"
          in
            compareP overTimeSpan testMe expectedResult

    describe "rand" $ do
      it "generates a (pseudo-)random number between zero & one" $ do
        it "at the start of a cycle" $
          (queryArc rand (Arc 0 0)) `shouldBe` [Event (Context []) Nothing (Arc 0 0) (0 :: Float)]
        it "at 1/4 of a cycle" $
          (queryArc rand (Arc 0.25 0.25)) `shouldBe`
            [Event (Context []) Nothing (Arc 0.25 0.25) (0.6295689214020967:: Float)]
        it "at 3/4 of a cycle" $
          (queryArc rand (Arc 0.75 0.75)) `shouldBe`
          [Event (Context []) Nothing (Arc 0.75 0.75) (0.20052618719637394 :: Float)]

    describe "irand" $ do
      it "generates a (pseudo-random) integer between zero & i" $ do
        it "at the start of a cycle" $
          (queryArc (irand 10) (Arc 0 0)) `shouldBe` [Event (Context []) Nothing (Arc 0 0) (0 :: Int)]
        it "at 1/4 of a cycle" $
          (queryArc (irand 10) (Arc 0.25 0.25)) `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (6 :: Int)]
        it "is patternable" $
          (queryArc (irand "10 2") (Arc 0 1)) `shouldBe` [
            Event (Context [((1,1),(3,1))]) Nothing (Arc 0 0.5) (6 :: Int), Event (Context [((4,1),(5,1))]) Nothing (Arc 0.5 1) (0 :: Int)
          ]
