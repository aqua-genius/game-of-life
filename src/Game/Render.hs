module Game.Render (
  renderC,
  renderContent,
  renderAliveCell,
  renderNonAliveCell,
  wrap,
) where

import Game.Core (Area(..), Cells, isAlive)

renderC :: Area -> Cells -> [String]
renderC = (wrap .) . renderContent

renderContent :: Area -> Cells -> [String]
renderContent (Area lowerX lowerY upperX upperY) cells =
  renderLine <$> [lowerY..upperY]
  where
    renderLine y = renderCell y =<< [lowerX..upperX]
    renderCell y x
      | isAlive cells (x, y) = renderAliveCell
      | otherwise = renderNonAliveCell

renderAliveCell :: String
renderAliveCell = "▓▓"

renderNonAliveCell :: String
renderNonAliveCell = "  "

wrap :: [String] -> [String]
wrap css = [topBorder] ++ (wrapVerticalSides <$> css) ++ [bottomBorder]
  where
    border = const '─' <$> head css
    topBorder = "┌" ++ border ++ "┐"
    bottomBorder = "└" ++ border ++ "┘"
    wrapVerticalSides cs = "│" ++ cs ++ "│"
