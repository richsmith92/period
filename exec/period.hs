{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

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
  | Expand {
    expandFmt :: PeriodFmt,
    expandInput :: T.Text,
    expandList :: Bool
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
runCommand (Collapse fmt per) = T.putStrLn $ collapsePeriod fmt $ parsePeriod per
runCommand (Expand fmt expandInput expandList) = if
  | expandList -> T.putStr . T.unlines . map (collapsePeriod fmt . (\x -> (x, x))) $ uncurry enumFromTo p
  | otherwise -> T.putStrLn $ formatPeriod fmt p
  where
  p = parsePeriod expandInput
