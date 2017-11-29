module Game.Core (
  Area(Area),
  Cells,
  Position,
  mutate,
  blink,
  createCells,
  isAlive,
  inside,
  positionsInside,
  neighbors,
) where

import Data.Set (Set, fromList, intersection, member, size)

data Area = Area { lowerX :: Int, lowerY :: Int, upperX :: Int, upperY :: Int }
  deriving (Eq, Show)

newtype Cells = Cells { aliveCells :: Set Position }
  deriving (Eq, Show)

type Position = (Int, Int)

mutate :: Area -> Cells -> Cells
mutate = flip $ (createCells .) . (. positionsInside) . filter . blink

blink :: Cells -> Position -> Bool
blink cells position = transitions . size . aliveNeighbors $ position
  where
    aliveNeighbors = intersection (aliveCells cells) . fromList . neighbors
    transitions 2 = isAlive cells position
    transitions 3 = True
    transitions _ = False

createCells :: [Position] -> Cells
createCells = Cells . fromList

isAlive :: Cells -> Position -> Bool
isAlive = flip member . aliveCells

inside :: Area -> Position -> Bool
inside (Area lowerX lowerY upperX upperY) (x, y) =
  lowerX <= x && x <= upperX && lowerY <= y && y <= upperY

positionsInside :: Area -> [Position]
positionsInside (Area lowerX lowerY upperX upperY) =
  (,) <$> [lowerX..upperX] <*> [lowerY..upperY]

neighbors :: Position -> [Position]
neighbors position@(x, y) =
  filter (/= position) $ positionsInside (Area (pred x) (pred y) (succ x) (succ y))
