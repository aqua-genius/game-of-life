module Game.IO (
  printC,
  printD,
  prepareScreen,
) where

import Control.Arrow (first)
import Data.Foldable (traverse_)
import Data.Tuple (swap)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Game.Core (Area(..), Cells, Difference(..))
import Game.Render (renderC, renderAliveCell, renderNonAliveCell)

printC :: Area -> Cells -> IO ()
printC = (traverse_ putStrLn .) . renderC

printD :: Area -> Difference -> IO ()
printD area (Difference cellsWillLive cellsWillDie) = do
  traverse_ printLC cellsWillLive
  traverse_ printDC cellsWillDie
  moveCursor $ underAreaPosition area
  where
    printLC = putAtPosition renderAliveCell
    printDC = putAtPosition renderNonAliveCell
    putAtPosition s = (>> putStr s) . moveCursor
    moveCursor = uncurry setCursorPosition . toCursorPosition
    toCursorPosition = swap . first (pred . (* 2))
    underAreaPosition (Area _ _ _ upperY) = (0, 2 + upperY)

prepareScreen :: IO ()
prepareScreen = clearScreen >> setCursorPosition 0 0
