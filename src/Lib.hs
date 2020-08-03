module Lib
  ( simplePso,
    inertiaWeightPso,
  )
where

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

data Particle = Particle
  { position :: Point,
    velocity :: Point,
    personalBestPosition :: Point
  }
  deriving (Show)

data Swarm = Swarm
  { particles :: [Particle],
    globalBestPosition :: Point
  }
  deriving (Show)

simplePso :: [Double] -> (Double, Double, [Double], [Double])
simplePso randoms = initSwarm simplePsoVelocity randoms

inertiaWeightPso :: [Double] -> (Double, Double, [Double], [Double])
inertiaWeightPso randoms = initSwarm inertiaWeightPsoVelocity randoms

initSwarm :: (Point -> Particle -> Double -> Double -> Point)
  -> [Double] -> (Double, Double, [Double], [Double])
initSwarm velocityPso randoms =
  (x finalPosition, y finalPosition, bestFitnessList, averageFitnessList)
  where
    fstList = take particleCount randoms
    sndList = take particleCount (drop particleCount randoms)
    positions = zipWith (\x y -> Point {x = x * 10 - 5, y = y * 10 - 5}) fstList sndList
    initParticles = map (\x -> Particle {position = x, velocity = Point {x = 0, y = 0},
      personalBestPosition = x}) positions
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) initParticles
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    initialSwarm = Swarm {particles = initParticles, globalBestPosition = curBest}
    dimensionRandoms = drop (2 * particleCount) randoms
    (finalSwarm, bestFitnessList, averageFitnessList) =
      iterateSwarm velocityPso initialSwarm dimensionRandoms iterations
    finalPosition = globalBestPosition finalSwarm
    particleCount = 200
    iterations = 250

iterateSwarm :: (Point -> Particle -> Double -> Double -> Point)
  -> Swarm -> [Double] -> Int -> (Swarm, [Double], [Double])
iterateSwarm _ swarm _ 0 = (swarm, [], [])
iterateSwarm velocityPso swarm (rand1 : rand2 : rest) iteration =
  (finalSwarm, bestFitnessList, averageFitnessList)
  where
    (updatedSwarm, best, avg) = updateSwarm velocityPso swarm rand1 rand2
    (finalSwarm, bestList, avgList) = iterateSwarm velocityPso updatedSwarm rest (iteration - 1)
    bestFitnessList = best : bestList
    averageFitnessList = avg : avgList
iterateSwarm _ _ _ _ = error "Random list must be infinite"

updateSwarm :: (Point -> Particle -> Double -> Double -> Point)
  -> Swarm -> Double -> Double -> (Swarm, Double, Double)
updateSwarm velocityPso (Swarm particles globalBestPosition) rand1 rand2 =
  (swarm, bestFitness, averageFitness)
  where
    swarm = Swarm {particles = par, globalBestPosition = gBest}
    par = map (\x -> updateParticle velocityPso globalBestPosition x rand1 rand2) particles
    gBest =
      if sixHumpCamelback curBest < sixHumpCamelback globalBestPosition
        then curBest
        else globalBestPosition
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) particles
    fitnessList = map (\x -> sixHumpCamelback (personalBestPosition x)) particles
    averageFitness = sum fitnessList / fromIntegral (length fitnessList)
    bestFitness = sixHumpCamelback gBest

updateParticle :: (Point -> Particle -> Double -> Double -> Point)
  -> Point -> Particle -> Double -> Double -> Particle
updateParticle velocityPso globalBestPosition particle rand1 rand2 =
  Particle {position = pos, velocity = vel, personalBestPosition = personalBest}
  where
    pos = position particle + velocity particle
    vel = velocityPso globalBestPosition particle rand1 rand2
    personalBest =
      if sixHumpCamelback pos < sixHumpCamelback oldPersonal
        then pos
        else oldPersonal
    oldPersonal = personalBestPosition particle

sixHumpCamelback :: Point -> Double
sixHumpCamelback (Point x y) =
  (4 - 2.1 * x ** 2 + (1 / 3) * x ** 4) * x ** 2 + x * y + ((-4) + 4 * y ** 2) * y ** 2

simplePsoVelocity :: Point -> Particle -> Double -> Double -> Point
simplePsoVelocity globalBestPosition particle rand1 rand2 =
  inertialPsoVelocity 1 globalBestPosition particle rand1 rand2

inertiaWeightPsoVelocity :: Point -> Particle -> Double -> Double -> Point
inertiaWeightPsoVelocity globalBestPosition particle rand1 rand2 =
  inertialPsoVelocity inertialFactor globalBestPosition particle rand1 rand2
  where
    inertialFactor = 0.792

inertialPsoVelocity :: Double -> Point -> Particle -> Double -> Double -> Point
inertialPsoVelocity weight globalBestPosition particle rand1 rand2 =
  ensureBounds computed maximumVelocity
  where
    computed = inertia + cognitiveComponent + socialComponent
    inertia = weight `mul` (velocity particle)
    cognitiveComponent = (cognitiveFactor * rand1) `mul` cognitiveDiff
    socialComponent = (socialFactor * rand2) `mul` socialDiff
    cognitiveDiff = personalBestPosition particle - position particle
    socialDiff = globalBestPosition - position particle
    cognitiveFactor = 1.4944
    socialFactor = cognitiveFactor
    maximumVelocity = 5
