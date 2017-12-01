module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Semigroup ((<>))
import System.IO (hFlush, stdout)
import Game.Core (mutate, difference)
import Game.IO (printC, printD, prepareScreen)
import Game.Read (readSeedContent)
import Options.Applicative

main :: IO ()
main = initialize =<< execParser options

initialize :: Args -> IO ()
initialize (Args seedFile interval singleRun) = do
  (area, cells) <- readSeedContent <$> readFile seedFile
  prepareScreen
  printC area cells
  unless singleRun $ loop area cells
  where
    loop area cells = do
      hFlush stdout
      threadDelay $ interval * 1000
      let nextCells = mutate area cells
      printD area $ difference cells nextCells
      loop area nextCells

data Args = Args { seedFile :: String, interval :: Int, singleRun :: Bool }

parser :: Parser Args
parser = Args <$>
  argument str (
    metavar "FILE" <>
    help "The seed"
  ) <*>
  option auto (
    short 'i' <>
    long "interval" <>
    metavar "INT" <>
    help "Interval of iterations in milliseconds" <>
    showDefault <>
    value 20
  ) <*>
  switch (
    long "single-run"
  )

options :: ParserInfo Args
options = info (parser <**> helper) $
  fullDesc <>
  header "Conway's Game of Life"
