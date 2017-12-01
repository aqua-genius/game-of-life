module Game.Core (
  Area(Area),
  Cells,
  Difference(Difference),
  Position,
  mutate,
  blink,
  difference,
  createCells,
  isAlive,
  inside,
  positionsInside,
  neighbors,
) where

import qualified Data.Set as Set

data Area = Area { lowerX :: Int, lowerY :: Int, upperX :: Int, upperY :: Int }
  deriving (Eq, Show)

newtype Cells = Cells { aliveCells :: Set.Set Position }
  deriving (Eq, Show)

data Difference = Difference { cellsWillLive :: Cells, cellsWillDie :: Cells }
  deriving (Eq, Show)

type Position = (Int, Int)

mutate :: Area -> Cells -> Cells
mutate = flip $ (createCells .) . (. positionsInside) . filter . blink

blink :: Cells -> Position -> Bool
blink cells position = transitions . Set.size . aliveNeighbors $ position
  where
    aliveNeighbors = Set.intersection (aliveCells cells) . Set.fromList . neighbors
    transitions 2 = isAlive cells position
    transitions 3 = True
    transitions _ = False

difference :: Cells -> Cells -> Difference
difference (Cells oldAliveCells) (Cells newAliveCells) =
  Difference cellsWillLive cellsWillDie
  where
    cellsWillLive = Cells $ Set.difference newAliveCells oldAliveCells
    cellsWillDie = Cells $ Set.difference oldAliveCells newAliveCells

createCells :: [Position] -> Cells
createCells = Cells . Set.fromList

isAlive :: Cells -> Position -> Bool
isAlive = flip Set.member . aliveCells

inside :: Area -> Position -> Bool
inside (Area lowerX lowerY upperX upperY) (x, y) =
  lowerX <= x && x <= upperX && lowerY <= y && y <= upperY

positionsInside :: Area -> [Position]
positionsInside (Area lowerX lowerY upperX upperY) =
  (,) <$> [lowerX..upperX] <*> [lowerY..upperY]

neighbors :: Position -> [Position]
neighbors position@(x, y) =
  filter (/= position) $ positionsInside (Area (pred x) (pred y) (succ x) (succ y))
