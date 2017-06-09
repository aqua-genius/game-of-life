{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Game.ReadSpec where

import Control.Arrow ((***))
import Data.Char (digitToInt)
import Game.Core
import Game.Read
import Test.QuickCheck

prop_readArea x y =
  readArea string == Area 1 1 x y
  where
    string = show x ++ ", " ++ show y

prop_readCells =
  readCells css == createCells [(1, 1), (2, 2), (3, 3)]
  where
    css = ["*", " *", "  *"]

prop_positioning =
  all check $ positioning css
  where
    check = uncurry (==) . (f *** digitToInt)
    f (x, y) = 3 * (y - 1) + x
    css = ["123", "456", "789"]

return []
main = $quickCheckAll
