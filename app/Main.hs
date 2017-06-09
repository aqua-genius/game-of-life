module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Options.Applicative
import Game.Core (mutate)
import Game.Read (readSeedContent)
import Game.Render (renderT)

main :: IO ()
main = initialize =<< execParser options

initialize :: Args -> IO ()
initialize (Args seedFile interval singleRun) = do
  (area, cells) <- readSeedContent <$> readFile seedFile
  printCells area cells
  unless singleRun $ loop area cells
  where printCells = (traverse putStrLn .) . renderT
        loop area cells = do
          printCells area cells
          threadDelay $ interval * 1000
          loop area $ mutate area cells

data Args = Args {
  seedFile :: String,
  interval :: Int,
  singleRun :: Bool
}

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
    help "Interval of iterations" <>
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
