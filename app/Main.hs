module Main where

import qualified Data.Text.IO as TextIO
import Data.Semigroup ((<>))
import Options.Applicative
import Game.Read (readSeedContent)

main :: IO ()
main = initialize =<< execParser options

initialize :: Args -> IO ()
initialize (Args seedFile interval singleRun) = do
  content <- TextIO.readFile seedFile
  let (area, cells) = readSeedContent content
  putStrLn $ "interval = " ++ show interval
  print area
  -- print cells

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
