{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Game.CoreSpec where

import Control.Monad (filterM)
import Game.Core
import Test.QuickCheck

instance Arbitrary Area where
  arbitrary = do
    lowerX <- choose (min, max)
    lowerY <- choose (min, max)
    upperX <- choose (lowerX, max)
    upperY <- choose (lowerY, max)
    return $ Area lowerX lowerY upperX upperY
    where
      (min, max) = (1, 10)

instance Arbitrary Cells where
  arbitrary = createCells <$> randomPositions
    where
      randomPositions =
        filterM (const randomState) =<< positionsInside <$> arbitrary
      randomState = frequency [(1, return True), (2, return False)]

prop_inside area@(Area lowerX lowerY upperX upperY) =
  inside area (lowerX, lowerY) && not (inside area (lowerX - 1, lowerY - 1))

prop_positionsInside area@(Area lowerX lowerY upperX upperY) =
  length (positionsInside area) == lengthExpected &&
  all (inside area) (positionsInside area)
  where
    lengthExpected = (upperX - lowerX + 1) * (upperY - lowerY + 1)

prop_neighbors position =
  length (neighbors position) == 8

return []
main = $quickCheckAll
