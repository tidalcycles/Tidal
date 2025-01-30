{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.StepwiseTest where

import qualified Data.Map.Strict as Map
import Data.Ratio
import Sound.Tidal.ParseBP
import Sound.Tidal.Pattern
import Sound.Tidal.Stepwise
import Test.Microspec
import TestUtils
import Prelude hiding ((*>), (<*))

run :: Microspec ()
run =
  describe "Sound.Tidal.Stepwise" $ do
    describe "stepcat" $ do
      it "can stepwise cat" $ do
        compareP (Arc 0 8) (stepcat ["a b c", "d e" :: Pattern String]) "a b c d e"
    describe "expand" $ do
      it "can pattern expands" $ do
        compareP (Arc 0 8) (expand "2 1" ("a b c" :: Pattern Char)) "a@2 b@2 c@2 a b c"
    describe "steptake" $ do
      it "can pattern takes" $ do
        compareP (Arc 0 8) (steptake "1 2 3 4" ("a b c d" :: Pattern Char)) "a a b a b c a b c d"
      it "can pattern reverse takes" $ do
        compareP (Arc 0 8) (steptake "-1 -2 -3 -4" ("a b c d" :: Pattern Char)) "d c d b c d a b c d"
    describe "stepdrop" $ do
      it "can pattern drops" $ do
        compareP (Arc 0 8) (stepdrop "0 1 2 3" ("a b c d" :: Pattern Char)) "a b c d a b c a b a"
      it "can pattern reverse drops" $ do
        compareP (Arc 0 8) (stepdrop "0 -1 -2 -3" ("a b c d" :: Pattern Char)) "a b c d b c d c d d"


--     describe('tactus', () => {
--   it('Is correctly preserved/calculated through transformations', () => {
--     expect(sequence(0, 1, 2, 3).linger(4).tactus).toStrictEqual(Fraction(4));
--     expect(sequence(0, 1, 2, 3).iter(4).tactus).toStrictEqual(Fraction(4));
--     expect(sequence(0, 1, 2, 3).fast(4).tactus).toStrictEqual(Fraction(4));
--     expect(sequence(0, 1, 2, 3).hurry(4).tactus).toStrictEqual(Fraction(4));
--     expect(sequence(0, 1, 2, 3).rev().tactus).toStrictEqual(Fraction(4));
--     expect(sequence(1).segment(10).tactus).toStrictEqual(Fraction(10));
--     expect(sequence(1, 0, 1).invert().tactus).toStrictEqual(Fraction(3));
--     expect(sequence({ s: 'bev' }, { s: 'amenbreak' }).chop(4).tactus).toStrictEqual(Fraction(8));
--     expect(sequence({ s: 'bev' }, { s: 'amenbreak' }).striate(4).tactus).toStrictEqual(Fraction(8));
--     expect(sequence({ s: 'bev' }, { s: 'amenbreak' }).slice(4, sequence(0, 1, 2, 3)).tactus).toStrictEqual(
--       Fraction(4),
--     );
--     expect(sequence({ s: 'bev' }, { s: 'amenbreak' }).splice(4, sequence(0, 1, 2, 3)).tactus).toStrictEqual(
--       Fraction(4),
--     );
--     expect(sequence({ n: 0 }, { n: 1 }, { n: 2 }).chop(4).tactus).toStrictEqual(Fraction(12));
--     expect(
--       pure((x) => x + 1)
--         .setTactus(3)
--         .appBoth(pure(1).setTactus(2)).tactus,
--     ).toStrictEqual(Fraction(6));
--     expect(
--       pure((x) => x + 1)
--         .setTactus(undefined)
--         .appBoth(pure(1).setTactus(2)).tactus,
--     ).toStrictEqual(Fraction(2));
--     expect(
--       pure((x) => x + 1)
--         .setTactus(3)
--         .appBoth(pure(1).setTactus(undefined)).tactus,
--     ).toStrictEqual(Fraction(3));
--     expect(stack(fastcat(0, 1, 2), fastcat(3, 4)).tactus).toStrictEqual(Fraction(6));
--     expect(stack(fastcat(0, 1, 2), fastcat(3, 4).setTactus(undefined)).tactus).toStrictEqual(Fraction(3));
--     expect(stackLeft(fastcat(0, 1, 2, 3), fastcat(3, 4)).tactus).toStrictEqual(Fraction(4));
--     expect(stackRight(fastcat(0, 1, 2), fastcat(3, 4)).tactus).toStrictEqual(Fraction(3));
--     // maybe this should double when they are either all even or all odd
--     expect(stackCentre(fastcat(0, 1, 2), fastcat(3, 4)).tactus).toStrictEqual(Fraction(3));
--     expect(fastcat(0, 1).ply(3).tactus).toStrictEqual(Fraction(6));
--     expect(fastcat(0, 1).setTactus(undefined).ply(3).tactus).toStrictEqual(undefined);
--     expect(fastcat(0, 1).fast(3).tactus).toStrictEqual(Fraction(2));
--     expect(fastcat(0, 1).setTactus(undefined).fast(3).tactus).toStrictEqual(undefined);
--   });
-- });
-- describe('stepcat', () => {
--   it('can cat', () => {
--     expect(sameFirst(stepcat(fastcat(0, 1, 2, 3), fastcat(4, 5)), fastcat(0, 1, 2, 3, 4, 5)));
--     expect(sameFirst(stepcat(pure(1), pure(2), pure(3)), fastcat(1, 2, 3)));
--   });
--   it('calculates undefined tactuses as the average', () => {
--     expect(sameFirst(stepcat(pure(1), pure(2), pure(3).setTactus(undefined)), fastcat(1, 2, 3)));
--   });
-- });
-- describe('taper', () => {
--   it('can taper', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).taper(1, 5), sequence(0, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 0, 1, 0)));
--   });
--   it('can taper backwards', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).taper(-1, 5), sequence(0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4)));
--   });
-- });
-- describe('increase and decrease', () => {
--   it('can increase from the left', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).increase(2), sequence(0, 1)));
--   });
--   it('can decrease to the left', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).decrease(2), sequence(0, 1, 2)));
--   });
--   it('can increase from the right', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).increase(-2), sequence(3, 4)));
--   });
--   it('can decrease to the right', () => {
--     expect(sameFirst(sequence(0, 1, 2, 3, 4).decrease(-2), sequence(2, 3, 4)));
--   });
--   it('can decrease nothing', () => {
--     expect(sameFirst(pure('a').decrease(0), pure('a')));
--   });
--   it('can decrease nothing, repeatedly', () => {
--     expect(sameFirst(pure('a').decrease(0, 0), fastcat('a', 'a')));
--     for (var i = 0; i < 100; ++i) {
--       expect(sameFirst(pure('a').decrease(...Array(i).fill(0)), fastcat(...Array(i).fill('a'))));
--     }
--   });
-- });
-- describe('expand', () => {
--   it('can expand four things in half', () => {
--     expect(
--       sameFirst(sequence(0, 1, 2, 3).expand(1, 0.5), stepcat(sequence(0, 1, 2, 3), sequence(0, 1, 2, 3).expand(0.5))),
--     );
--   });
--   it('can expand five things in half', () => {
--     expect(
--       sameFirst(
--         sequence(0, 1, 2, 3, 4).expand(1, 0.5),
--         stepcat(sequence(0, 1, 2, 3, 4), sequence(0, 1, 2, 3, 4).expand(0.5)),
--       ),
--     );
--   });
-- });
-- describe('stepJoin', () => {
--   it('can join a pattern with a tactus of 2', () => {
--     expect(
--       sameFirst(
--         sequence(pure(pure('a')), pure(pure('b').setTactus(2))).stepJoin(),
--         stepcat(pure('a'), pure('b').setTactus(2)),
--       ),
--     );
--   });
--   it('can join a pattern with a tactus of 0.5', () => {
--     expect(
--       sameFirst(
--         sequence(pure(pure('a')), pure(pure('b').setTactus(0.5))).stepJoin(),
--         stepcat(pure('a'), pure('b').setTactus(0.5)),
--       ),
--     );
--   });
-- });