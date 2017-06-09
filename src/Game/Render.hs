module Game.Render (
  renderT,
  render,
  wrap,
) where

import Game.Core (Area(..), Cells, isAlive)

renderT :: Area -> Cells -> [String]
renderT = (((clearScreenCode :) . wrap) .) . render

render :: Area -> Cells -> [String]
render (Area lowerX lowerY upperX upperY) cells =
  renderLine <$> [lowerY..upperY]
  where renderLine y = renderCell y =<< [lowerX..upperX]
        renderCell y x = if isAlive cells (x, y) then renderAliveCell else renderNonAliveCell
        renderNonAliveCell = "  "
        renderAliveCell = "▓▓"

wrap :: [String] -> [String]
wrap css = [topBorder] ++ (wrapVerticalSides <$> css) ++ [bottomBorder]
  where border = replicate (length . head $ css) '─'
        topBorder = "┌" ++ border ++ "┐"
        bottomBorder = "└" ++ border ++ "┘"
        wrapVerticalSides cs = "│" ++ cs ++ "│"

clearScreenCode :: String
clearScreenCode = "\ESC[2J\ESC[3J\ESC[H"
