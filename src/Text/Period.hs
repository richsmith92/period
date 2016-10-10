{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Text.Period
 ( Period, PeriodFmt(..), parsePeriod, parsePeriodMay, parsePeriodEither
 , formatPeriod, collapsePeriod
 )where

import           Control.Applicative hiding ((<|>))
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Monoid         ((<>))
import qualified Data.Text           as T

#if MIN_VERSION_time(1, 5, 0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import Data.Time.Calendar
import Data.Time (formatTime)

import Prelude
import TextShow (showt)

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text


type Period = (Day, Day)

data PeriodFmt = PeriodFmt
  { perFieldSep :: T.Text -- ^ Separator between year, month and day
  , perDateSep  :: T.Text -- ^ Separator between dates in the range, e.g. comma
                          -- in @yyyy-mm-dd,yyyy-mm-dd@
  }

data ParseState
  = StateYear Integer
  | StateMonth Integer Int
  | StateDay Integer Int Int
  | StateNone

number :: (Read a) => Int -> Parser a
number n = read <$> count n digit

skipFieldSep :: Stream s m Char => Bool -> T.Text -> ParsecT s u m ()
skipFieldSep b sep = when b (string (T.unpack sep)>> return ())

-- | Parse period from text representation, which can be either simple date
-- range (@yyyy-mm-dd,yyyy-mm-dd@) or something shorter like @yyyy,yyyy@, or even
-- something like @yyyy-01-23,03-21@ which corresponds to
-- @yyyy-01-23,yyyy-03-21@. The list of possible combinations can be found in
-- @test/Spec.hs@.
--
-- 'parsePeriod' understands dash as field separator (like @yyyy-mm-dd@), or
-- accepts short date format (@yyyymmdd@). It
-- understands comma and underscore as separators between
-- dashed dates (@yyyy-mm-dd@) and dash as separator between short dates (@yyyymmdd@).
--
-- 'parsePeriod' produces error on unparsable input.
parsePeriod :: T.Text -> Period
parsePeriod = either (error . show) id . parse period "parsePeriod"

-- | Safe analogue of 'parsePeriod' using Maybe
parsePeriodMay :: T.Text -> Maybe Period
parsePeriodMay = either (const Nothing) Just . parse period ""

-- | Safe analogue of 'parsePeriod' using Either
parsePeriodEither :: T.Text -> Either String Period
parsePeriodEither = either (Left . show) Right . parse period "parsePeriod"

period :: Parser Period
period =
  try (rangePeriod '-' "") <|>
  try (rangePeriod '_' "-") <|>
  try (rangePeriod ',' "-") <|>
  try (primPeriod "" <* eof) <|>
  try (primPeriod "-" <* eof) <|>
  (quarter <* eof)

rangePeriod :: Char -> T.Text -> Parser Period
rangePeriod sep fmt = do
  s1 <- prim StateNone True
  _ <- char sep
  s2 <- foldr (<|>) (prim StateNone True <* eof) $
    [try (prim s False <* eof) | s <- states s1]
  return (startDay s1, endDay s2)
  where
  prim = primPeriod' fmt
  states (StateMonth y _) = [StateYear y]
  states (StateDay y m _) = [StateMonth y m, StateYear y]
  states _ = []

startDay :: ParseState -> Day
startDay StateNone = error "startDay StateNone"
startDay (StateYear y) = fromGregorian y 1 1
startDay (StateMonth y m) = fromGregorian y m 1
startDay (StateDay y m d) = fromGregorian y m d

endDay :: ParseState -> Day
endDay StateNone = error "endDay StateNone"
endDay (StateYear y) = fromGregorian y 12 31
endDay (StateMonth y m) = fromGregorian y m 31
endDay (StateDay y m d) = fromGregorian y m d

primPeriod :: T.Text -> Parser Period
primPeriod fmt = (startDay &&& endDay) <$> primPeriod' fmt StateNone True

primPeriod' :: T.Text -> ParseState -> Bool -> Parser ParseState
primPeriod' fmt StateNone _ = do
  s <- StateYear <$> number 4
  primPeriod' fmt s True <|> return s
primPeriod' fmt (StateYear y) skip = do
  skipFieldSep skip fmt
  s <- StateMonth y <$> number 2
  primPeriod' fmt s True <|> return s
primPeriod' fmt (StateMonth y m) skip = do
  skipFieldSep skip fmt
  StateDay y m <$> number 2
primPeriod' _ (StateDay _ _ _) _ = unexpected "primPeriod': StateDay"

quarter :: Parser Period
quarter = do
  y <- number 4
  _ <- char 'Q'
  q <- digitToInt <$> digit
  return (fromGregorian y (q * 3 - 2) 1, fromGregorian y (q * 3) 31)

-- | Format a period in the shortest fashion, e.g. collapse @yyyy-01-01,yyyy-01-31@
-- to @yyyy-01@
collapsePeriod :: PeriodFmt -> Period -> T.Text
collapsePeriod (PeriodFmt fieldSep sep) (start, end) = if
  | m1 == 1, d1 == 1, m2 == 12, d2 == 31 -> if
      | y1 == y2 -> showt y1
      | otherwise -> showt y1 <> sep <> showt y2
  | d1 == 1, end == monthEnd start -> format yyyymm start
  | d1 == 1, end == quarterEnd start -> showt y1 <> "Q" <> showt q
  | start == end -> format yyyymmdd start
  | otherwise -> format yyyymmdd start <> sep <> format yyyymmdd end
  where
  format f = T.pack . formatTime defaultTimeLocale f
  (y1, m1, d1) = toGregorian start
  (y2, m2, d2) = toGregorian end
  q = (m1 - 1) `div` 3 + 1
  yyyymm = T.unpack $ "%Y" <> fieldSep <> "%m"
  yyyymmdd = T.unpack $ "%Y" <> fieldSep <> "%m" <> fieldSep <> "%d"

-- | Format a period as a simple date range, e.g. @yyyy-mm-dd,yyyy-mm-dd@
formatPeriod :: PeriodFmt -> Period -> T.Text
formatPeriod (PeriodFmt fieldSep sep) (start, end) =
  format start <> sep <> format end
  where
  format = T.pack . formatTime defaultTimeLocale yyyymmdd
  yyyymmdd = T.unpack $ "%Y" <> fieldSep <> "%m" <> fieldSep <> "%d"

monthEnd :: Day -> Day
monthEnd = pred . addGregorianMonthsClip 1

quarterEnd :: Day -> Day
quarterEnd =  pred . addGregorianMonthsClip 3
