{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Game.CoreSpec where

import Test.QuickCheck
import Game.Core

prop_inside lowerX lowerY upperX upperY =
  lowerX <= upperX && lowerY <= upperY ==>
    inside area (lowerX, lowerY) &&
    not (inside area (lowerX - 1, lowerY - 1))
  where area = Area lowerX lowerY upperX upperY

prop_positionsInside lowerX lowerY upperX upperY =
  lowerX <= upperX && lowerY <= upperY ==>
    length (positionsInside area) == lengthExpected &&
      all (inside area) (positionsInside area)
  where lengthExpected = (upperX - lowerX + 1) * (upperY - lowerY + 1)
        area = Area lowerX lowerY upperX upperY

prop_neighbors position =
  length (neighbors position) == 8

return []
main = $quickCheckAll
