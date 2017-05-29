{-# LANGUAGE TemplateHaskell #-}
module Flock
  ( 
  -- * Sensor configuration
  -- ** Type
    Sensor (..)
  -- ** Constructor
  , mkSensor
  -- ** Lenses
  , sensorRange, sensorRayAngle, sensorRaySect, sensorPointRadius
  -- * Agents
  -- ** Type
  , Agent (..)
  -- ** Constructor
  , mkAgent
  -- ** Control functions
  , move, scan, turn, turnTowards, neighbours, nearObstacles
  -- ** Lenses
  , agentPosition, agentRadius, agentDirection, agentSpeed
  , agentSensor, agentFlocking, agentAtDest
  -- * Obstacles
  -- ** Type
  , Obstacle (..)
  -- ** Lenses
  , obstRadius, obstPosition
  -- * Plane
  -- ** Type
  , Plane (..)
  -- ** Control functions
  , step, stepR
  -- ** Lenses
  , planeObstacles, planeAgents
  -- * Object Info
  , Info (..)
  -- * Geometry
  , Angle, Distance, Level
  -- * Rendering
  , Render (..)
  -- * DEBUG
  , sensorPoints, Behavior
  )
  where

import Control.Lens hiding (Level)
import Control.Applicative ((<|>))
import Control.Monad.Random (Rand, runRand)
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.List (find, sortBy)
import Data.Function (on)

import Graphics.Gloss.Data.Picture -- (Point)
import Graphics.Gloss.Data.Vector -- (Vector, normalizeV)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Geometry.Angle

import System.Random (StdGen)

type Angle = Float
type Distance = Float

data Sensor = Sensor
  { _sensorRange       :: Distance
  , _sensorRayAngle    :: Angle
  , _sensorRaySect     :: Distance
  , _sensorPointRadius :: Distance
  }
  deriving(Show, Eq)

data Agent = Agent
  { _agentPosition  :: Point   -- ^ Position of the agent on the plane
  , _agentRadius    :: Float   -- ^ Radius of the agent
  , _agentDirection :: Vector  -- ^ Direction, in which the agent is heading
  , _agentSpeed     :: Float   -- ^ Speed at which the agent is moving
  , _agentSensor    :: Sensor  -- ^ Sensor range of the agent
  , _agentFlocking  :: Bool    -- ^ Is the agent in a flock?
  , _agentAtDest    :: Bool    -- ^ Is the agent or its flock at its destination?
  } deriving (Show, Eq)

data Obstacle = Obstacle
  { _obstPosition :: Point    -- ^ The position of the obstacle on the plane
  , _obstRadius   :: Float    -- ^ The radius of the obstacle
  } deriving (Show, Eq)

data Plane = Plane
  { _planeAgents    :: [Agent]
  , _planeObstacles :: [Obstacle]
  } deriving (Show, Eq)

type Behavior a = ReaderT Plane (StateT Agent (Rand StdGen)) a

dist :: Point -> Point -> Float
dist (x,y) (x',y') = sqrt ((x'-x)^2 + (y'-y)^2)

runBehavior :: Behavior a -> StdGen -> Plane -> Agent -> (a,Agent,StdGen)
runBehavior action gen plane agent = (a,agent',gen')
  where
    agentM' = runStateT (runReaderT action plane) agent
    ((a,agent'),gen') = runRand agentM' gen


makeLenses ''Sensor

-- | Constructs a sensor config
mkSensor :: Int     -- ^ Number of scans per ray
         -> Int     -- ^ Number of rays
         -> Float   -- ^ Ray length
         -> Float   -- ^ Radius of a scan point
         -> Sensor
mkSensor s r l r'= Sensor l ((2*pi) / toEnum r) (l / toEnum s) r'

-- | Agents are circular autonomous mobile objects, searching of there destination.

makeLenses ''Agent

-- | Constructs an agent
mkAgent :: Point  -- ^ Starting position
        -> Float  -- ^ Radius of the agent (size)
        -> Vector -- ^ Initial direction
        -> Float  -- ^ Initial speed
        -> Sensor -- ^ Sensor config
        -> Agent
mkAgent p r d s f = Agent p r d s f False False

-- | Moves an agent, according to its state
move' :: Agent -> Agent
move' agent = agent { _agentPosition = p }
 where
  (dx,dy) = agent
          & _agentDirection
          & normalizeV
          & both %~ (* (_agentSpeed agent))
  (x,y) = _agentPosition agent
  p = (x+dx,y+dy)

-- | Moves an agent, according to its state
move :: Behavior ()
move = modify move'

-- | Turns an agent by an angle
turn' :: Angle -> Agent -> Agent
turn' a agent
  | notANumber = agent
  | otherwise  = agent'
 where
  agent' = agent & agentDirection %~ (rotateV a . normalizeV)
  notANumber = agent'
             & _agentDirection
             & both %~ isNaN
             & uncurry (||)


-- | Turns an agent by an angle
turn :: Angle -> Behavior ()
turn = modify . turn'

-- | Lets the agent directly head to a specific point.
turnTowards' :: Point -> Agent -> Agent
turnTowards' (tx,ty) agent = agent { _agentDirection = d' }
 where
  (x,y) = _agentPosition agent
  d' = (tx-x, ty-y)
     & normalizeV

-- | Lets the agent directly head to a specific point.
turnTowards :: Point -> Behavior ()
turnTowards = modify . turnTowards'

-- | Obstacles are circular objects, which can not be passed by any agent.

makeLenses ''Obstacle


data Info = AGENT | OBSTACLE
  deriving (Show, Eq)

makeLenses ''Plane

type Ray a = (Angle, a)

-- | Calculates the front and back sensor rays, cast by an agent,
--   @without@ position offset.
sensorRays :: Agent -> ([Ray Vector],[Ray Vector])
sensorRays agent = (asf, asb)
 where
  Sensor _ a _ _ = _agentSensor agent
  -- Scanning begins on the left
  d = normalizeV $ rotateV (pi/2) $ _agentDirection agent
  asf = map (fmap $ flip rotateV d) [ (a',a') | a' <- [0, (-a) .. (-pi)]]
  asb = map (fmap $ flip rotateV d) [ (a',a') | a' <- [0, a .. pi]]

sensorPoints :: Agent -> Vector -> [Point]
sensorPoints agent ray = [ r'' `mulSV` ray
                         | f <- factors
                         , let r'' = r' * f -- Radius r'' of a scanpoint
                         ]
 where
  Sensor r _ r' d = _agentSensor agent
  ar = _agentRadius agent
  factors = [1..]
          & dropWhile (\f -> f*r'-d < ar)
          & takeWhile (\f -> f*r' <= r)

scannedPositions :: Agent -> ([Ray [Point]],[Ray [Point]])
scannedPositions agent = field' & both.mapped %~ fmap (map (\(x,y) -> (x+px,y+py)))
 where
  field = sensorRays agent
  field' = field & both.mapped %~ fmap (sensorPoints agent)
  (px, py) = _agentPosition agent

checkPoint :: Plane -> Point -> Distance -> Maybe Info
checkPoint plane p d = obst <|> agents
 where
  dist (x,y) (x',y') = magV (x'-x,y'-y) - d
  obst = const OBSTACLE <$> find
    (\o -> dist p (_obstPosition o) <= (_obstRadius o))
    (_planeObstacles plane)
  agents = const AGENT <$> find
    (\a -> dist p (_agentPosition a) <= (_agentRadius a))
    (_planeAgents plane)

type Level = Int


scan' :: Plane -> Distance -> Ray [Point] -> Maybe (Level, Angle, Info)
scan' plane d (a,ps) = do
  (l,i) <- scan'' 1 ps
  return (l,a,i)
 where
  scan'' :: Int -> [Point] -> Maybe (Level, Info)
  scan'' _ []     = Nothing
  scan'' i (p:ps) = ((,) i <$> checkPoint plane p d) <|> scan'' (i+1) ps

-- | Scans the plane around an agent
scanP :: Plane                  -- ^ The plane, in which the agent exists
     -> Agent                  -- ^ The Agent
     -> ([Maybe (Level, Angle, Info)] -- Objects in front of the agent
        ,[Maybe (Level, Angle, Info)] -- Objects behind the agent
        )
scanP p a = scannedPositions a & both.mapped %~ scan' p (a^.agentSensor.sensorPointRadius)
 where
  (front, back) = scannedPositions a

-- | Scans the plane around an agent
scan :: Behavior [(Level, Angle, Info)]
scan = do
  agent <- get
  plane <- ask
  return
    $ catMaybes
    $ uncurry (++)
    $ scanP plane agent

neighbours :: Float -> Behavior [Agent]
neighbours dMax = do
  p <- use agentPosition
  r <- use agentRadius
  filter (\a -> dist (_agentPosition a) p < dMax - (r + _agentRadius a)) <$> view planeAgents

nearObstacles :: Float -> Behavior [Obstacle]
nearObstacles dMax = do
  p <- use agentPosition
  r <- use agentRadius
  filter (\o -> dist (_obstPosition o) p < dMax - (r + _obstRadius o)) <$> view planeObstacles

-- | Simulates the plane by one step according to a function,
--   which defines the behavior of an agent in the plane.
step :: (Plane -> Agent -> Agent)   -- ^ Behavior function.
     -> Plane                       -- ^ Plane
     -> Plane
step f p = p & planeAgents %~ map (f p)

-- | As step, but in a random monad.
stepR :: (Plane -> Agent -> Rand g Agent)
      -> Plane
      -> Rand g Plane
stepR f p = do
  as <- mapM (f p) (_planeAgents p)
  return p{_planeAgents = as}

class Render a where
  render :: Plane -> a -> Picture

instance Render Agent where
  render plane a = pictures [a', dir]
   where
    fl = _agentFlocking a
    at = _agentAtDest a
    c | fl && at  = green
      | fl        = blue
      |       at  = yellow
      | otherwise = red

    a' = translate (a^.agentPosition._1) (a^.agentPosition._2)
       $ color c
       $ circleSolid (_agentRadius a)

    s' = color black
       $ pts

    Sensor r agl r' pr = _agentSensor a

    getSense [] = error "getSense [] happend"
    getSense [p] = p
    getSense (p:ps) = case checkPoint plane p (a^.agentSensor.sensorRange) of
      Nothing -> getSense ps
      Just _ -> p


    cross = pictures [line [(-pr,0),(pr,0)],line [(0,-pr),(0,pr)]]
    rays = a
        & scannedPositions
        & uncurry (++)
        & map snd
        & map (sortBy (compare `on` (dist (a^.agentPosition))))
        & map reverse
        & map getSense
        & map (\(x,y) -> cross & translate x y)
        & pictures
    pts = a
        & scannedPositions
        & uncurry (++)
        & map snd
        & mapped.mapped %~ (\(x,y) -> cross & translate x y)
        & map pictures
        & pictures
    dir = a
        & _agentDirection
        & (\d@(x,y) -> pictures [line [(0,0)
                                      , (r + 10) `mulSV` normalizeV d]
                                , translate x y cross])
        & color green
        & translate (a^.agentPosition._1) (a^.agentPosition._2)



instance Render Obstacle where
  render plane o = translate (o^.obstPosition._1) (o^.obstPosition._2)
                 $ color black
                 $ circleSolid (_obstRadius o)

instance Render Plane where
  render _ p = pictures
    $ map (render p) (p^.planeAgents) 
    ++ map (render p) (p^.planeObstacles)
    {-
    ++ [p^.planeAgents
        & map (\x -> (_agentPosition x,_agentDirection x))
        & map show
        & map text
        & map (scale 0.1 0.1)
        & zip [1..]
        & map (\(i,p) -> translate 0 (i*20) p)
        & pictures
        & translate (-1000) 0
        ]
    -}
    ++ [circle 100
        & translate 100 100
        & color blue
        ]




