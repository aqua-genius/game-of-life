module Game.Core (
  Area(..),
  Cells(..),
  Position,
  mutate,
  blink,
  createCells,
  isAlive,
  inside,
  positionsInside,
  neighbors,
) where

import Data.Set (Set, fromList)

data Area = Area { lowerX :: Int, lowerY :: Int, upperX :: Int, upperY :: Int }
  deriving (Eq, Show)

newtype Cells = Cells { alive :: Set Position }
  deriving (Eq, Show)

type Position = (Int, Int)

mutate :: Area -> Cells -> Cells
mutate = undefined

blink :: Cells -> Position -> Bool
blink = undefined

createCells :: [Position] -> Cells
createCells = Cells . fromList

isAlive :: Cells -> Position -> Bool
isAlive = undefined

inside :: Area -> Position -> Bool
inside (Area lowerX lowerY upperX upperY) (x, y) =
  lowerX <= x && x <= upperX && lowerY <= y && y <= upperY

positionsInside :: Area -> [Position]
positionsInside (Area lowerX lowerY upperX upperY) =
  [(x, y) | x <- [lowerX..upperX], y <- [lowerY..upperY]]

neighbors :: Position -> [Position]
neighbors position@(x, y) =
  filter (/= position) $ positionsInside (Area (x - 1) (y - 1) (x + 1) (y + 1))
