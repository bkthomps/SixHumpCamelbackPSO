module Main where

import Lib
import System.Random (newStdGen, randomRs)

main :: IO ()
main = do
  list <- randomList
  writePso "1_simple_pso" (simplePso list)
  writePso "2_inertia_weight_pso" (inertiaWeightPso list)
  writePso "3_constrict_coefficient_pso" (constrictionCoefficientPso list)
  writePso "4_guaranteed_convergence_pso" (simplePso list)

randomList :: IO [Double]
randomList = randomRs (0.0, 1.0) <$> newStdGen

writePso :: String -> (Double, Double, [Double], [Double]) -> IO ()
writePso name (x, y, best, avg) = do
  writeFile (name ++ "_solution.csv") (displayPair x y)
  writeFile (name ++ "_best.csv") ("sep=,\n" ++ displayList best)
  writeFile (name ++ "_average.csv") ("sep=,\n" ++ displayList avg)

displayPair :: Double -> Double -> String
displayPair x y = "(" ++ show x ++ ", " ++ show y ++ ")\n"

displayList :: [Double] -> String
displayList (first : list) = show first ++ "\n" ++ displayList(list)
displayList _ = []
