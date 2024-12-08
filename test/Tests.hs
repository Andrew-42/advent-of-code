module Main where

import GridSpec (gridSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    gridSpec
