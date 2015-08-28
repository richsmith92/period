{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import           Text.Period

import Prelude

main :: IO ()
main = execParser (info (helper <*> optParser) $ progDesc "period") >>= runCommand

data Command
  = Collapse PeriodFmt T.Text
  | Expand PeriodFmt T.Text

periodArg :: Parser T.Text
periodArg = argument (T.pack <$> str) (metavar "PERIOD")

parseFmt :: Parser PeriodFmt
parseFmt = PeriodFmt <$>
  option (T.pack <$> str) (
    short 'f' <> long "field-sep" <> value "-" <>
    help "Field separator, by default '-' as in 'yyyy-mm-dd'") <*>
  option (T.pack <$> str) (
    short 'd' <> long "date-sep" <> value "," <>
    help "Field separator, by default ',' as in 'yyyy-mm-dd,yyyy-mm-dd'")


mkCommand :: String -> String -> Parser Command -> Mod CommandFields Command
mkCommand cmd desc parser =
  command cmd $ info (helper <*> parser) (progDesc desc)

optParser :: Parser Command
optParser = subparser $ mconcat $
  [ mkCommand "collapse" "Find shortest representation for time period" $
    Collapse <$> parseFmt <*> periodArg
  , mkCommand "expand" "Display start and end points of time period" $
    Expand <$> parseFmt <*> periodArg
  ]

runCommand :: Command -> IO ()
runCommand (Collapse fmt per) = T.putStrLn $ collapsePeriod fmt $ parsePeriod per
runCommand (Expand fmt per) = T.putStrLn $ formatPeriod fmt $ parsePeriod per
