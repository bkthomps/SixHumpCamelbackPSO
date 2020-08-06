module Lib
  ( simplePso,
    inertiaWeightPso,
    constrictionCoefficientPso,
    guaranteedConvergencePso,
  )
where

import Point

data Particle = Particle
  { position :: Point,
    velocity :: Point,
    personalBest :: Point
  }
  deriving (Show)

data Swarm = Swarm
  { particles :: [Particle],
    globalBest :: Point,
    successCount :: Int,
    failureCount :: Int,
    scalingFactor :: Double
  }
  deriving (Show)

type UpdateEquations = (Swarm -> Particle -> Double -> Double -> (Point, Point))

simplePso :: [Double] -> (Double, Double, [Double], [Double])
simplePso randoms = initSwarm simplePsoUpdate randoms

inertiaWeightPso :: [Double] -> (Double, Double, [Double], [Double])
inertiaWeightPso randoms = initSwarm inertiaPsoWeightUpdate randoms

constrictionCoefficientPso :: [Double] -> (Double, Double, [Double], [Double])
constrictionCoefficientPso randoms = initSwarm constrictionCoefficientPsoUpdate randoms

guaranteedConvergencePso :: [Double] -> (Double, Double, [Double], [Double])
guaranteedConvergencePso randoms = initSwarm guaranteedConvergencePsoUpdate randoms

initSwarm :: UpdateEquations -> [Double] -> (Double, Double, [Double], [Double])
initSwarm velocityPso randoms =
  (x finalPosition, y finalPosition, bestFitnessList, averageFitnessList)
  where
    fstList = take particleCount randoms
    sndList = take particleCount (drop particleCount randoms)
    positions = zipWith (\x y -> Point {x = x * 10 - 5, y = y * 10 - 5}) fstList sndList
    initParticles = map (\x -> Particle {position = x, velocity = Point {x = 0, y = 0},
      personalBest = x}) positions
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) initParticles
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    initialSwarm = Swarm {particles = initParticles, globalBest = curBest,
      successCount = 0, failureCount = 0, scalingFactor = 1}
    dimensionRandoms = drop (2 * particleCount) randoms
    (finalSwarm, bestFitnessList, averageFitnessList) =
      iterateSwarm velocityPso initialSwarm dimensionRandoms iterations
    finalPosition = globalBest finalSwarm
    particleCount = 200
    iterations = 250

iterateSwarm :: UpdateEquations -> Swarm -> [Double] -> Int -> (Swarm, [Double], [Double])
iterateSwarm _ swarm _ 0 = (swarm, [], [])
iterateSwarm velocityPso swarm (rand1 : rand2 : rest) iteration =
  (finalSwarm, bestFitnessList, averageFitnessList)
  where
    (updatedSwarm, best, avg) = updateSwarm velocityPso swarm rand1 rand2
    (finalSwarm, bestList, avgList) = iterateSwarm velocityPso updatedSwarm rest (iteration - 1)
    bestFitnessList = best : bestList
    averageFitnessList = avg : avgList
iterateSwarm _ _ _ _ = error "Random list must be infinite"

updateSwarm :: UpdateEquations -> Swarm -> Double -> Double -> (Swarm, Double, Double)
updateSwarm velocityPso swarm rand1 rand2 = (updatedSwarm, bestFitness, averageFitness)
  where
    updatedSwarm = Swarm {particles = par, globalBest = gBest,
      successCount = updatedSuccess, failureCount = updatedFailure,
      scalingFactor = updatedScale}
    par = map (\x -> updateParticle velocityPso swarm x rand1 rand2) (particles swarm)
    gBest =
      if sixHumpCamelback curBest < sixHumpCamelback (globalBest swarm)
        then curBest
        else (globalBest swarm)
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) (particles swarm)
    fitnessList = map (\x -> sixHumpCamelback (personalBest x)) (particles swarm)
    averageFitness = sum fitnessList / fromIntegral (length fitnessList)
    bestFitness = sixHumpCamelback gBest
    updatedSuccess = if gBest == (globalBest swarm) then 0 else (successCount swarm) + 1
    updatedFailure = if gBest == (globalBest swarm) then (failureCount swarm) + 1 else 0
    updatedScale = (scalingFactor swarm) *
      if (successCount swarm) > successThreshold then 2
      else (if (failureCount swarm) > failureThreshold then 0.5 else 1)
    successThreshold = 15
    failureThreshold = 5

updateParticle :: UpdateEquations -> Swarm -> Particle -> Double -> Double -> Particle
updateParticle velocityPso swarm particle rand1 rand2 =
  Particle {position = pos, velocity = vel, personalBest = updatedPersonalBest}
  where
    (pos, vel) = velocityPso swarm particle rand1 rand2
    updatedPersonalBest =
      if sixHumpCamelback pos < sixHumpCamelback oldPersonal
        then pos
        else oldPersonal
    oldPersonal = personalBest particle

sixHumpCamelback :: Point -> Double
sixHumpCamelback (Point x y) =
  (4 - 2.1 * x ** 2 + (1 / 3) * x ** 4) * x ** 2 + x * y + ((-4) + 4 * y ** 2) * y ** 2

genericPsoUpdate :: (Double, Double, Double) -> UpdateEquations
genericPsoUpdate (inertialFactor, cognitiveFactor, socialFactor)
  swarm particle rand1 rand2 = (pos, ensureBounds computed maximumVelocity)
  where
    pos = position particle + velocity particle
    computed = inertia + cognitiveComponent + socialComponent
    inertia = inertialFactor `mul` (velocity particle)
    cognitiveComponent = (cognitiveFactor * rand1) `mul` cognitiveDiff
    socialComponent = (socialFactor * rand2) `mul` socialDiff
    cognitiveDiff = personalBest particle - position particle
    socialDiff = (globalBest swarm) - position particle
    maximumVelocity = 1

simplePsoUpdate :: UpdateEquations
simplePsoUpdate swarm particle rand1 rand2 =
  genericPsoUpdate factors swarm particle rand1 rand2
  where
    factors = (inertialFactor, cognitiveFactor, socialFactor)
    inertialFactor = 1
    cognitiveFactor = 1.4944
    socialFactor = cognitiveFactor

inertiaPsoWeightUpdate :: UpdateEquations
inertiaPsoWeightUpdate swarm particle rand1 rand2 =
  genericPsoUpdate factors swarm particle rand1 rand2
  where
    factors = (inertialFactor, cognitiveFactor, socialFactor)
    inertialFactor = 0.792
    cognitiveFactor = 1.4944
    socialFactor = cognitiveFactor

constrictionCoefficientPsoUpdate :: UpdateEquations
constrictionCoefficientPsoUpdate swarm particle rand1 rand2 =
  genericPsoUpdate factors swarm particle rand1 rand2
  where
    factors = (constrictionFactor * inertialFactor,
      constrictionFactor * cognitiveFactor,
      constrictionFactor * socialFactor)
    inertialFactor = 1
    cognitiveFactor = 2.05
    socialFactor = cognitiveFactor
    constrictionFactor = 2 / abs (2 - phi - sqrt (phi ** 2 - 4 * phi))
    phi = cognitiveFactor + socialFactor

guaranteedConvergencePsoUpdate :: UpdateEquations
guaranteedConvergencePsoUpdate swarm particle rand1 rand2
  | (globalBest swarm) == (position particle) = (pos, vel)
  | otherwise = inertiaPsoWeightUpdate swarm particle rand1 rand2
  where
    pos = globalBest swarm + inertia + sample
    vel = pos - position particle
    inertia = inertialFactor `mul` (velocity particle)
    sample = Point {x = sampleSize, y = sampleSize}
    sampleSize = (scalingFactor swarm) * (1 - 2 * rand2)
    inertialFactor = 0.792
