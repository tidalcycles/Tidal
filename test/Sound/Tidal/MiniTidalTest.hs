{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.MiniTidalTest where

import Test.Microspec hiding (run)
import Sound.Tidal.MiniTidal
import Sound.Tidal.Context as Tidal
import Data.Either
import qualified Data.Map.Strict as Map

parsesTo :: String -> ControlPattern -> Property
parsesTo str p = x `shouldBe` y
  where x = query <$> miniTidal str <*> Right (State (Arc 0 16) Map.empty)
        y = Right $ query p $ State (Arc 0 16) Map.empty

causesParseError :: String -> Property
causesParseError str = isLeft (miniTidal str :: Either String ControlPattern) `shouldBe` True

run :: Microspec ()
run =
  describe "miniTidal" $ do

    it "parses the empty string as silence" $
      "" `parsesTo` silence

    it "parses a string containing only spaces as silence" $
      "    " `parsesTo` silence

    it "parses the identifier silence as silence" $
      "silence" `parsesTo` silence

    it "parses a very simple single 's' pattern" $
      "s \"bd cp\"" `parsesTo` s "bd cp"

    it "parses a very simple single 'sound' pattern" $
      "sound \"bd cp\"" `parsesTo` sound "bd cp"

    it "parses a single 's' pattern that uses angle brackets" $
      "s \"<bd cp>\"" `parsesTo` s "<bd cp>"

    it "parses a single 's' pattern that uses square brackets" $
      "s \"[bd sn] cp\"" `parsesTo` s "[bd sn] cp"

    it "parses a single 's' pattern that uses square brackets and *" $
      "s \"[bd sn]*2 cp\"" `parsesTo` s "[bd sn]*2 cp"

    it "parses a single 's' pattern that uses Bjorklund rhythms" $
      "s \"sn(5,16)\"" `parsesTo` s "sn(5,16)"

    it "parses a literal int as a double pattern" $
      "pan 0" `parsesTo` (pan 0)

    it "parses a literal double as a double pattern" $
      "pan 1.0" `parsesTo` (pan 1.0)

    it "parses a negative literal double as a double pattern" $
      "pan (-1.0)" `parsesTo` (pan (-1.0))

    it "parses two merged patterns" $
      "s \"bd cp\" # pan \"0 1\"" `parsesTo` (s "bd cp" # pan "0 1")

    it "parses three merged patterns" $
      "s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\"" `parsesTo`
        (s "bd cp" # pan "0 1" # gain "0.5 0.7")

    it "parses three merged patterns, everything in brackets" $
      "(s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\")" `parsesTo`
        ((s "bd cp" # pan "0 1" # gain "0.5 0.7"))

    it "parses three merged patterns, everything in muliple layers of brackets" $
      "(((s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\")))" `parsesTo`
        ((((s "bd cp" # pan "0 1" # gain "0.5 0.7"))))

    it "parses three merged patterns with right associative brackets" $
      "s \"bd cp\" # (pan \"0 1\" # gain \"0.5 0.7\")" `parsesTo`
        (s "bd cp" # (pan "0 1" # gain "0.5 0.7"))

    it "parses three merged patterns with left associative brackets" $
      "(s \"bd cp\" # pan \"0 1\") # gain \"0.5 0.7\"" `parsesTo`
        ((s "bd cp" # pan "0 1") # gain "0.5 0.7")

    it "parses simple patterns in brackets applied to ParamPattern functions" $
      "s (\"bd cp\")" `parsesTo` (s ("bd cp"))

    it "parses simple patterns applied to ParamPattern functions with $" $
      "s $ \"bd cp\"" `parsesTo` (s $ "bd cp")

    it "parses addition of simple patterns" $
      "n (\"0 1\" + \"2 3\")" `parsesTo` (n ("0 1" + "2 3"))

    it "parses multiplication of simple patterns as a merged parampattern" $
      "s \"arpy*8\" # up (\"3\" * \"2\")" `parsesTo` (s "arpy*8" # up ("3" * "2"))

    it "parses pan patterns" $
      "pan \"0 0.25 0.5 0.75 1\"" `parsesTo` (pan "0 0.25 0.5 0.75 1")

    it "parses note patterns" $
      "note \"0 0.25 0.5 0.75 1\"" `parsesTo` (note "0 0.25 0.5 0.75 1")

    it "parses sine oscillators" $
      "pan sine" `parsesTo` (pan sine)

    it "parses sine oscillators used in pan patterns" $
      "s \"arpy*8\" # pan sine" `parsesTo` (s "arpy*8" # pan sine)

    it "parses striate transformations of s patterns" $
      "striate 8 $ s \"arpy*8\"" `parsesTo` (striate 8 $ s "arpy*8")

    it "parses chop transformations of s patterns" $
      "chop 8 $ s \"arpy*8\"" `parsesTo` (chop 8 $ s "arpy*8")

    it "parses fast transformations of parampatterns" $
      "fast 2 $ s \"bd cp\"" `parsesTo` (fast 2 $ s "bd cp")

    it "parses fast transformations of parampatterns when in brackets" $
      "(fast 2) $ s \"bd cp\"" `parsesTo` ((fast 2) $ s "bd cp")

    it "parses rev transformations of parampatterns" $
      "rev $ s \"bd cp\"" `parsesTo` (rev $ s "bd cp")

    it "parses rev transformations of parampatterns when in brackets" $
      "(rev) $ s \"bd cp\"" `parsesTo` ((rev) $ s "bd cp")

    it "parses jux transformations with transformations in brackets" $
        "jux (rev) $ s \"arpy*8\" # up \"0 2 3 5 3 5 7 8\"" `parsesTo`
         (jux (rev) $ s "arpy*8" # up "0 2 3 5 3 5 7 8")

    it "parses jux transformations with transformations not in brackets" $
        "jux rev $ s \"arpy*8\" # up \"0 2 3 5 3 5 7 8\"" `parsesTo`
         (jux rev $ s "arpy*8" # up "0 2 3 5 3 5 7 8")

    it "doesn't parse when a transformation requiring an argument is provided without parens or $ to jux" $
      causesParseError "jux fast 2 $ s \"bd*4 cp\""

    it "parses multiple fast transformations of parampatterns" $
      "fast 2 $ fast 2 $ s \"bd cp\"" `parsesTo` (fast 2 $ fast 2 $ s "bd cp")

    it "parses an 'every' transformation applied to a simple s pattern" $
      "every 2 (fast 2) (s \"bd cp\")" `parsesTo` (every 2 (fast 2) (s "bd cp"))

    it "parses a transformed pattern merged with a pattern constructed from parampatterning an arithmetic expression on patterns" $
      "(every 2 (fast 2) $ s \"arpy*8\") # up (\"[0 4 7 2,16 12 12 16]\" - \"<0 3 5 7>\")" `parsesTo` ((every 2 (fast 2) $ s "arpy*8") # up ("[0 4 7 2,16 12 12 16]" - "<0 3 5 7>"))

    it "parses a fast transformation applied to a simple (ie. non-param) pattern" $
      "up (fast 2 \"<0 2 3 5>\")" `parsesTo`
        (up (fast 2 "<0 2 3 5>"))

    it "parses a partially-applied pattern transformation spread over patterns" $
      "spread (fast) [2,1,1.5] $ s \"bd sn cp sn\"" `parsesTo`
        (spread (fast) [2,1,1.5] $ s "bd sn cp sn")

    it "parses a binary Num function spread over a simple Num pattern" $
      "n (spread (+) [2,3,4] \"1 2 3\")" `parsesTo`
        (n (spread (+) [2,3,4] "1 2 3"))

    it "parses an $ application spread over partially applied transformations of a non-Control Pattern" $
      "n (spread ($) [density 2, rev, slow 2] $ \"1 2 3 4\")" `parsesTo`
        (n (spread ($) [density 2, rev, slow 2] $ "1 2 3 4"))

    it "parses an $ application spread over transformations of a control pattern" $
      "spread ($) [fast 2,fast 4] $ s \"bd cp\"" `parsesTo`
        (spread ($) [fast 2,fast 4] $ s "bd cp")

    it "parses an $ application spread over partially applied transformations of a Control Pattern" $
      "spread ($) [density 2, rev, slow 2, striate 3] $ sound \"[bd*2 [~ bd]] [sn future]*2 cp jvbass*4\"" `parsesTo`
        (spread ($) [density 2, rev, slow 2, striate 3] $ sound "[bd*2 [~ bd]] [sn future]*2 cp jvbass*4")

    it "parses an off transformation" $
      "off 0.125 (fast 2) $ s \"bd sn cp glitch\"" `parsesTo`
        (off 0.125 (fast 2) $ s "bd sn cp glitch")

    it "parses a pattern rotation operator (1)" $
      "0.25 <~ (s \"bd sn cp glitch\")" `parsesTo`
        (0.25 <~ (s "bd sn cp glitch"))

    it "parses a pattern rotation operator (2)" $
      "0.25 <~ s \"bd sn cp glitch\"" `parsesTo`
        (0.25 <~ s "bd sn cp glitch")

    it "parses a pattern rotation operator (3)" $
      "\"0.25 0.125 0 0.5\" <~ s \"bd sn cp glitch\"" `parsesTo`
        ("0.25 0.125 0 0.5" <~ s "bd sn cp glitch")

    it "parses a pattern rotation operator (3) applied to a transformation with $" $
      "fast 4 $ \"<0 [0.125,0.25]>\" <~ s \"bd cp sn glitch:2\"" `parsesTo`
        (fast 4 $ "<0 [0.125,0.25]>" <~ s "bd cp sn glitch:2")

    it "parses a left section transformation of a controlpattern" $
      "every 2 (s \"arpy*8\" #) $ s \"drum\"" `parsesTo`
        (every 2 (s "arpy*8" #) $ s "drum")

    it "parses a right section transformation of a controlpattern" $
      "every 2 (# n \"3 4\") $ s \"drum\"" `parsesTo`
        (every 2 (# n "3 4") $ s "drum")

    it "parses right sections in a list with spread" $
      "spread ($) [(# n \"4 5 6 7\"),(# n \"0 1 2 3\")] $ s \"drum*4\"" `parsesTo`
            (spread ($) [(# n "4 5 6 7"),(# n "0 1 2 3")] $ s "drum*4")

    it "parses pattern merges spread with #" $
       "spread (#) [n \"4 5 6 7\",n \"0 1 2 3\"] $ s \"drum*4\"" `parsesTo`
        (spread (#) [n "4 5 6 7",n "0 1 2 3"] $ s "drum*4")

    it "parses a left section pattern rotation operator in an every expression" $
       "every 2 (0.0625 ~>) $ (0.5 ~>) $ s \"snare\"" `parsesTo`
        (every 2 (0.0625 ~>) $ (0.5 ~>) $ s "snare")

    it "parses a right section |> operator in an every expression" $
       "every 2 (|> speed \"2\") $ sound \"arpy*4\" |> speed \"1\"" `parsesTo`
        (every 2 (|> speed "2") $ sound "arpy*4" |> speed "1")

    it "parses a complex expression with multiple every, left sections and |>" $
       "every 3 (|- note \"3\") $ every 2 (|+ up \"5\") $ sound \"arpy*4\" |> note \"0 2 4 5\"" `parsesTo`
        (every 3 (|- note "3") $ every 2 (|+ up "5") $ sound "arpy*4" |> note "0 2 4 5")

    it "parses an expression with run" $
       "up (run 12) # sound \"arpy\"" `parsesTo`
        (up (Tidal.run 12) # sound "arpy")

    it "parses an expression with range" $
       "sound \"bd*8 sn*8\" # speed (range 1 3 $ tri)" `parsesTo`
       (sound "bd*8 sn*8" # speed (range 1 3 $ tri))

    it "parses a rotation operator with BP pattern as left argument" $
       "\"[0 0.25]/4\" <~ (sound \"bd*2 cp*2 hh sn\")" `parsesTo`
       ("[0 0.25]/4" <~ (sound "bd*2 cp*2 hh sn"))

    it "parses a sometimesBy application with a right section" $
       "sometimesBy 0.75 (# crush 4) $ sound \"bd arpy sn ~\"" `parsesTo`
       (sometimesBy 0.75 (# crush 4) $ sound "bd arpy sn ~")

    it "parses a whenmod application" $
       "whenmod 8 6 (rev) $ sound \"bd*2 arpy*2 cp hh*4\"" `parsesTo`
       (whenmod 8 6 (rev) $ sound "bd*2 arpy*2 cp hh*4")

    it "parses a complex example with const" $
       "every 12 (const $ sound \"bd*4 sn*2\") $ sound \"bd sn bass2 sn\"" `parsesTo`
       (every 12 (const $ sound "bd*4 sn*2") $ sound "bd sn bass2 sn")

    it "parses an example with fastcat" $
       "fastcat [sound \"bd sn:2\" # vowel \"[a o]/2\", sound \"casio casio:1 casio:2*2\"]" `parsesTo`
       (fastcat [sound "bd sn:2" # vowel "[a o]/2",sound "casio casio:1 casio:2*2"])

    it "parses an example with stack" $
       "stack [s \"bd cp\",s \"arpy*8\"]" `parsesTo`
       (stack [s "bd cp",s "arpy*8"])

    it "parses an example with samples and cut" $
       "sound (samples \"arpy*8\" (run 8)) # speed \"0.25\" # cut \"1\"" `parsesTo`
       (sound (samples "arpy*8" (Tidal.run 8)) # speed "0.25" # cut "1")
