module Main where

import GridSpec (gridSpec)
import Test.Hspec (hspec)
import UtilsSpec (utilsSpec)

main :: IO ()
main = hspec $ do
    gridSpec
    utilsSpec
