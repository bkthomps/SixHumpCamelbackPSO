module Main where

import Lib
import System.Random (newStdGen, randomRs)

main :: IO ()
main = do
  list <- randomList
  let (x_1, y_1, best_1, avg_1) = simplePso list
  print ((show x_1) ++ " " ++ (show y_1))
  print (show best_1)
  print (show avg_1)
  print ""
  let (x_2, y_2, best_2, avg_2) = inertiaWeightPso list
  print ((show x_2) ++ " " ++ (show y_2))
  print (show best_2)
  print (show avg_2)
  print ""
  let (x_3, y_3, best_3, avg_3) = constrictionCoefficientPso list
  print ((show x_3) ++ " " ++ (show y_3))
  print (show best_3)
  print (show avg_3)
  print ""
  let (x_4, y_4, best_4, avg_4) = guaranteedConvergencePso list
  print ((show x_4) ++ " " ++ (show y_4))
  print (show best_4)
  print (show avg_4)

randomList :: IO [Double]
randomList = randomRs (0.0, 1.0) <$> newStdGen
