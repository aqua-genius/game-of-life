module Game.CoreTest where

import Control.Monad (filterM)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Game.Core
import Game.Read

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
      randomPositions = filterM (const randomState) =<< positionsInside <$> arbitrary
      randomState = frequency [(1, return True), (2, return False)]

caseMutate = testCase "mutate" $ do
  mutate (Area 1 1 4 4) (readCells s11) @?= readCells s11
  mutate (Area 1 1 5 5) (readCells s21) @?= readCells s22
  mutate (Area 1 1 5 5) (readCells s22) @?= readCells s21
  where
    s11 = lines "     \n\
                \ **  \n\
                \ * * \n\
                \  *  \n\
                \     \n"
    s21 = lines "     \n\
                \  *  \n\
                \  *  \n\
                \  *  \n\
                \     \n"
    s22 = lines "     \n\
                \     \n\
                \ *** \n\
                \     \n\
                \     \n"

caseBlink = testCase "blink" $ do
  blink (readCells s10) (2, 2) @?= False
  blink (readCells s01) (2, 2) @?= False
  blink (readCells s02) (2, 2) @?= False
  blink (readCells s12) (2, 2) @?= True
  blink (readCells s03) (2, 2) @?= True
  blink (readCells s15) (2, 2) @?= False
  where
    s10 = lines "   \n\
                \ * \n\
                \   \n"
    s01 = lines "   \n\
                \*  \n\
                \   \n"
    s02 = lines "   \n\
                \* *\n\
                \   \n"
    s12 = lines "   \n\
                \***\n\
                \   \n"
    s03 = lines " * \n\
                \*  \n\
                \ * \n"
    s15 = lines "* *\n\
                \ * \n\
                \* *\n"

caseDifference = testCase "difference" $
  difference oldCells newCells @?= Difference cellsWillLive cellsWillDie
  where
    oldCells = createCells [(1, 1), (2, 2)]
    newCells = createCells [(1, 2), (2, 2)]
    cellsWillLive = createCells [(1, 2)]
    cellsWillDie = createCells [(1, 1)]

propInside = testProperty "inside" $
  \area@(Area lowerX lowerY upperX upperY) ->
    inside area (lowerX, lowerY) &&
    inside area (upperX, upperY) &&
    inside area (lowerX, upperY) &&
    inside area (upperX, lowerY) &&
    not (inside area (pred lowerX, lowerY)) &&
    not (inside area (lowerX, pred lowerY)) &&
    not (inside area (succ upperX, upperY)) &&
    not (inside area (upperX, succ upperY))

propPositionsInside = testProperty "positionsInside" $
  \area ->
    length (positionsInside area) == lengthExpected area &&
    all (inside area) (positionsInside area)
    where
      lengthExpected (Area lowerX lowerY upperX upperY) =
        (succ upperX - lowerX) * (succ upperY - lowerY)

propNeighbors = testProperty "neighbors" $
  \position ->
    length (neighbors position) == 8

props = testGroup "Properties" [propInside, propPositionsInside, propNeighbors]

cases = testGroup "Unit Tests" [caseMutate, caseBlink, caseDifference]

tests = testGroup "Game.Core" [props, cases]
