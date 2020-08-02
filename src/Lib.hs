module Lib
  ( simplePso,
  )
where

simplePso :: IO ()
simplePso = print (sixHumpCamelback 4 2)

sixHumpCamelback :: Double -> Double -> Double
sixHumpCamelback x y
  | max (abs x) (abs y) > 5 = error "Bad camelback range"
  | otherwise = (4 - 2.1 * x ^ 2 + (1 / 3) * x ^ 4) * x ^ 2 + x * y + ((-4) + 4 * y ^ 2) * y ^ 2
