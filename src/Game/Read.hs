module Game.Read (
  readArea,
  readCells,
  readSeedContent,
) where

import qualified Data.Text as Text
import Control.Arrow ((***), (&&&))
import Game.Core (Area(..), Cells(..))

readArea :: Text.Text -> Area
readArea = uncurry (Area 1 1) . firstTwo . readInts
  where readInts = map readInt . Text.splitOn (Text.pack ",")
        readInt = read . Text.unpack . Text.strip
        firstTwo (x : y : _) = (x, y)

readCells :: [Text.Text] -> Cells
readCells = undefined

readSeedContent :: Text.Text -> (Area, Cells)
readSeedContent = (readArea *** readCells) . (head &&& tail) . Text.lines
