{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import           Text.Period
import Text.PrettyPrint.ANSI.Leijen (string)
import Prelude

main :: IO ()
main = execParser (info (helper <*> optParser) $ progDescDoc (Just $ string desc)) >>= runCommand
  where
  desc = "Expand or collapse text representation of date range.\n\
  \\n\
  \period collapse 2018-01-01,2018-12-31\n\
  \=> 2018\n\n\
  \period collapse 2018-01-01,2118-12-31\n\
  \=> 2018,2118\n\n\
  \period expand 2018-01\n\
  \=> 2018-01-01,2018-01-31\n\n\
  \period expand 2018-01-07,14\n\
  \=> 2018-01-07,2018-01-14\
  \"

data Command
  = Collapse {
    colFmt :: PeriodFmt,
    colInput :: T.Text
  } | Expand {
    expFmt :: PeriodFmt,
    expInput :: T.Text,
    expList :: Bool
  }

periodArg :: Parser T.Text
periodArg = argument (T.pack <$> str) (metavar "PERIOD")

parseFmt :: Parser PeriodFmt
parseFmt = PeriodFmt <$>
  option (T.pack <$> str) (
    short 'f' <> long "field-sep" <> value "-" <>
    help "Field separator, by default '-' as in 'yyyy-mm-dd'") <*>
  option (T.pack <$> str) (
    short 'd' <> long "date-sep" <> value "," <>
    help "Start and end separator, by default ',' as in 'yyyy-mm-dd,yyyy-mm-dd'")

mkCommand :: String -> String -> Parser Command -> Mod CommandFields Command
mkCommand cmd desc parser =
  command cmd $ info (helper <*> parser) (progDesc desc)

optParser :: Parser Command
optParser = subparser $ mconcat $
  [ mkCommand "collapse" "Find shortest representation for time period" $
    Collapse <$> parseFmt <*> periodArg
  , mkCommand "expand" "Display start and end points of time period" $
    Expand <$> parseFmt <*> periodArg <*> switch (long "list" <> help "List all days in the period")
  ]

runCommand :: Command -> IO ()
runCommand Collapse{..} = T.putStrLn $ collapsePeriod colFmt $ parsePeriod colInput
runCommand Expand{..} = if
  | expList -> T.putStr . T.unlines . map (collapsePeriod expFmt . (\x -> (x, x))) $ uncurry enumFromTo p
  | otherwise -> T.putStrLn $ formatPeriod expFmt p
  where
  p = parsePeriod expInput
