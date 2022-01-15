{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import qualified RIO.Text as T

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12

run :: RIO App ()
run = do
  app <- ask
  let opts = (appOptions app)
  case opts of
       [] -> runRIO app runAll
       xs -> mapM_ (\n -> runRIO app $ runDay_ n) xs

errorDay :: Text -> RIO App ()
errorDay x = do
  logError $ (display x) <> " is an invalid day."

runDay_ :: Text -> RIO App ()
runDay_ x = case readMaybe $ T.unpack x of
                 Nothing -> errorDay x
                 Just n -> runDay n

runDay :: Int -> RIO App ()
runDay 1 = day1
runDay 2 = day2
runDay 3 = day3
runDay 4 = day4
runDay 5 = day5
runDay 6 = day6
runDay 7 = day7
runDay 8 = day8
runDay 9 = day9
runDay 10 = day10
runDay 11 = day11
runDay 12 = day12
runDay n = errorDay $ tshow n

runAll :: RIO App ()
runAll = mapM_ runDay [1..12]
