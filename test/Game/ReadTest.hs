module Game.ReadTest where

import Control.Arrow ((***))
import Data.Char (digitToInt)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Game.Core
import Game.Read

propReadArea = testProperty "readArea" $
  \x y ->
    readArea (show x ++ ", " ++ show y) == Area 1 1 x y

caseReadCells = testCase "readCells" $
  readCells s @?= createCells [(1, 1), (2, 2), (3, 3)]
  where
    s = lines "*  \n\
              \ * \n\
              \  *\n"

casePositioning = testCase "positioning" $
  all check (positioning s) @? []
  where
    check = uncurry (==) . (f *** digitToInt)
    f (x, y) = 3 * pred y + x
    s = lines "123\n\
              \456\n\
              \789\n"

props = testGroup "Properties" [propReadArea]

cases = testGroup "Unit Tests" [caseReadCells, casePositioning]

tests = testGroup "Game.Read" [props, cases]
