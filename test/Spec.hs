{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit ((@?=))
import qualified Data.Text as T
import Text.Period

main :: IO ()
main = hspec $ do
    describe "Primitive" $ do
      parse "2015" (2015,01,01) (2015,12,31)
      parse "201501" (2015,01,01) (2015,01,31)
      parse "20150131" (2015,01,31) (2015,01,31)
      parse "2015-01" (2015,01,01) (2015,01,31)
      parse "2015-01-31" (2015,01,31) (2015,01,31)
    describe "Quarters" $ do
      parse "2015Q1" (2015,01,01) (2015,03,31)
      parse "2015Q2" (2015,04,01) (2015,06,31)
      parse "2015Q3" (2015,07,01) (2015,09,31)
      parse "2015Q4" (2015,10,01) (2015,12,31)
    describe "Simple ranges" $ do
      parse "2014_2015" (2014,01,01) (2015,12,31)
      parse "2014-2015" (2014,01,01) (2015,12,31)
      parse "201406-201502" (2014,06,01) (2015,02,28)
      parse "2014-06_2015-02" (2014,06,01) (2015,02,28)
      parse "20140601-20150314" (2014,06,01) (2015,03,14)
      parse "2014-06-01_2015-03-14" (2014,06,01) (2015,03,14)
    describe "Ranges within same year" $ do
      parse "2015-02_05" (2015,02,01) (2015,05,31)
      parse "2015-02-22_05-11" (2015,02,22) (2015,05,11)
    describe "Ranges within same month" $ do
      parse "2015-01-22_31" (2015,01,22) (2015,01,31)
    describe "Unparsable" $ do
      failOn "201501_201502"
      failOn "20140601_20150314"

  where
  parse per (y1,m1,d1) (y2,m2,d2) = specify per $
    parsePeriod (T.pack per) @?=(fromGregorian y1 m1 d1, fromGregorian y2 m2 d2)
  failOn per = specify per $ parsePeriodMay (T.pack per) @?= Nothing


