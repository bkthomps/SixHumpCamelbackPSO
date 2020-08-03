module Main where

import Lib
import System.Random (newStdGen, randomRs)

main :: IO ()
main = do
  list <- randomList
  let (x, y, best, avg) = simplePso list
  print ((show x) ++ " " ++ (show y))
  print (show best)
  print (show avg)

randomList :: IO [Double]
randomList = randomRs (0.0, 1.0) <$> newStdGen
