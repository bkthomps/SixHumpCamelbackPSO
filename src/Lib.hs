module Lib
  ( simplePso,
  )
where

data Point = Point
  { x :: Double,
    y :: Double
  }
  deriving (Show)

point :: Double -> Double -> Point
point x y = Point {x = xBounded, y = yBounded}
  where
    xBounded = x `min` bound `max` (- bound)
    yBounded = y `min` bound `max` (- bound)
    bound = 5 -- TODO: this checks velocity too

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = point (x1 + x2) (y1 + y2)
  (-) (Point x1 y1) (Point x2 y2) = point (x1 - x2) (y1 - y2)

mul :: Double -> Point -> Point
mul scalar (Point x y) = point (scalar * x) (scalar * y)

ensureBounds :: Double -> Point -> Point
ensureBounds bound (Point x y) = point xBounded yBounded
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

simplePso :: [Double] -> Double
simplePso = initSwarm

initSwarm :: [Double] -> Double
initSwarm randoms = sixHumpCamelback finalPosition
  where
    fstList = take particleCount randoms
    sndList = take particleCount (drop particleCount randoms)
    positions = zipWith (\x y -> point (x * 10 - 5) (y * 10 - 5)) fstList sndList
    initParticles = map (\x -> Particle x (point 0 0) x) positions
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) initParticles
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    initialSwarm = Swarm {particles = initParticles, globalBestPosition = curBest}
    dimensionRandoms = drop (2 * particleCount) randoms
    finalSwarm = iterateSwarm initialSwarm dimensionRandoms iterations
    finalPosition = globalBestPosition finalSwarm
    particleCount = 200
    iterations = 250

iterateSwarm :: Swarm -> [Double] -> Int -> Swarm
iterateSwarm swarm _ 0 = swarm
iterateSwarm swarm (rand1 : rand2 : rest) iteration =
  iterateSwarm (updateSwarm swarm rand1 rand2) rest (iteration - 1)
iterateSwarm _ _ _ = error "Random list must be infinite"

updateSwarm :: Swarm -> Double -> Double -> Swarm
updateSwarm (Swarm particles globalBestPosition) rand1 rand2 =
  Swarm {particles = par, globalBestPosition = gBest}
  where
    par = map (\x -> updateParticle globalBestPosition x rand1 rand2) particles
    gBest =
      if sixHumpCamelback curBest < sixHumpCamelback globalBestPosition
        then curBest
        else globalBestPosition
    curBest = fst (foldl1 (\x y -> if snd x < snd y then x else y) pairs)
    pairs = map (\x -> (position x, sixHumpCamelback (position x))) particles

updateParticle :: Point -> Particle -> Double -> Double -> Particle
updateParticle globalBestPosition particle rand1 rand2 =
  Particle {position = pos, velocity = vel, personalBestPosition = personalBest}
  where
    pos = position particle + velocity particle
    vel = simplePsoVelocity globalBestPosition particle rand1 rand2
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
  ensureBounds maximumVelocity computed
  where
    computed = inertia + cognitiveComponent + socialComponent
    inertia = velocity particle
    cognitiveComponent = (cognitiveFactor * rand1) `mul` cognitiveDiff
    socialComponent = (socialFactor * rand2) `mul` socialDiff
    cognitiveDiff = personalBestPosition particle - position particle
    socialDiff = globalBestPosition - position particle
    cognitiveFactor = 1.4944
    socialFactor = cognitiveFactor
    maximumVelocity = 5
