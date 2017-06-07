module Main where

import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = initialize =<< execParser options

initialize :: Args -> IO ()
initialize (Args seedFile interval singleRun) = do
  putStrLn $ "seed-file-name = " ++ show seedFile
  putStrLn $ "interval = " ++ show interval
  putStrLn $ "singleRun = " ++ show singleRun

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
