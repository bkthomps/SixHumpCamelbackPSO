module Main where

import Lib
import System.Random (newStdGen, randomRs)

main :: IO ()
main = do
  list <- randomList
  print (simplePso list)

randomList :: IO [Double]
randomList = randomRs (0.0, 1.0) <$> newStdGen
