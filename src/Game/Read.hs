module Game.Read (
  readSeedContent,
  readArea,
  readCells,
  positioning,
) where

import Control.Arrow ((&&&), (***))
import Data.Text (pack, splitOn, strip, unpack)
import Game.Core (Area(..), Cells, Position, createCells)

readSeedContent :: String -> (Area, Cells)
readSeedContent = (readArea *** readCells) . (head &&& tail) . lines

readArea :: String -> Area
readArea = uncurry (Area 1 1) . firstTwo . readInts . pack
  where
    readInts = map readInt . splitOn separator
    readInt = read . unpack . strip
    firstTwo (x : y : _) = (x, y)
    separator = pack ","

readCells :: [String] -> Cells
readCells = createCells . map fst . filter (representAliveCell . snd) . positioning

positioning :: [String] -> [(Position, Char)]
positioning css = do
  (y, cs) <- zip [1..] css
  (x, c) <- zip [1..] cs
  return ((x, y), c)

representAliveCell :: Char -> Bool
representAliveCell = (== '*')
