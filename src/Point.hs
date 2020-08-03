module Point where

data Point = Point
  { x :: Double,
    y :: Double
  }
  deriving (Show)

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = Point {x = x1 + x2, y = y1 + y2}
  (-) (Point x1 y1) (Point x2 y2) = Point {x = x1 - x2, y = y1 - y2}
  (*) _ _ = undefined
  abs _ = undefined
  signum _ = undefined
  fromInteger _ = undefined

mul :: Double -> Point -> Point
mul scalar (Point x y) = Point {x = scalar * x, y = scalar * y}

ensureBounds :: Point -> Double -> Point
ensureBounds (Point x y) bound = Point {x = xBounded, y = yBounded}
  where
    xBounded = x `min` bound `max` (- bound)
    yBounded = y `min` bound `max` (- bound)
