module Game.Read (
  readArea,
  readCells,
  readSeedContent,
) where

import Control.Arrow ((***), (&&&))
import Data.Set (fromList)
import Data.Text (pack, unpack, strip, splitOn)
import Game.Core (Area(..), Cells(..), Position)

readArea :: String -> Area
readArea = uncurry (Area 1 1) . firstTwo . readInts . pack
  where readInts = map readInt . splitOn separator
        readInt = read . unpack . strip
        firstTwo (x : y : _) = (x, y)
        separator = pack ","

readCells :: [String] -> Cells
readCells = Cells . fromList . map fst .
  filter (representAliveCell . snd) . withPositions

withPositions :: [String] -> [(Position, Char)]
withPositions css = do
  (y, cs) <- zip [1..] css
  (x, c) <- zip [1..] cs
  return ((x, y), c)

representAliveCell :: Char -> Bool
representAliveCell = (== '*')

readSeedContent :: String -> (Area, Cells)
readSeedContent = (readArea *** readCells) . (head &&& tail) . lines
