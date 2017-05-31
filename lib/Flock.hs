{-# LANGUAGE TemplateHaskell #-}
module Flock
  ( 
  -- * Behavior Monad
    Behavior
  , runBehavior
  -- * Sensor configuration
  -- ** Type
  , Sensor (..)
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
  , move, scan, turn, turnTowards --, neighbours, nearObstacles
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
  -- * Geometry
  , Angle, Distance --, Level
  -- * Rendering
  , Render (..)
  -- * DEBUG
  -- , sensorPoints
  )
  where

import Control.Lens
  ( makeLenses
  , (^.), (&), use, (%~), _1, _2, both, view)

import Control.Monad.Random (Rand, runRand)

import Control.Monad.Reader

import Control.Monad.State


import Graphics.Gloss.Data.Picture -- (Point)
import Graphics.Gloss.Data.Vector -- (Vector, normalizeV)
import Graphics.Gloss.Data.Color

import System.Random (StdGen)


--------------------------------------------------------------------------------
-- Data ------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Angle = Float
type Distance = Float

data Sensor = Sensor
  { _sensorRange       :: Distance
  , _sensorRayAngle    :: Angle
  , _sensorRaySect     :: Distance
  , _sensorPointRadius :: Distance
  }
  deriving(Show, Eq)

-- | Agents are circular autonomous mobile objects, searching of there destination.
data Agent = Agent
  { _agentPosition  :: Point   -- ^ Position of the agent on the plane
  , _agentRadius    :: Float   -- ^ Radius of the agent
  , _agentDirection :: Vector  -- ^ Direction, in which the agent is heading
  , _agentSpeed     :: Float   -- ^ Speed at which the agent is moving
  , _agentSensor    :: Sensor  -- ^ Sensor range of the agent
  , _agentFlocking  :: Bool    -- ^ Is the agent in a flock?
  , _agentAtDest    :: Bool    -- ^ Is the agent or its flock at its destination?
  } deriving (Show, Eq)

-- | Obstacles are circular objects, which can not be passed by any agent.
data Obstacle = Obstacle
  { _obstPosition :: Point    -- ^ The position of the obstacle on the plane
  , _obstRadius   :: Float    -- ^ The radius of the obstacle
  } deriving (Show, Eq)

data Plane = Plane
  { _planeAgents    :: [Agent]
  , _planeObstacles :: [Obstacle]
  } deriving (Show, Eq)

type Behavior a = ReaderT Plane (StateT Agent (Rand StdGen)) a

makeLenses ''Sensor
makeLenses ''Agent
makeLenses ''Obstacle
makeLenses ''Plane

dist :: Point -> Point -> Float
dist (x,y) (x',y') = sqrt ((x'-x)^2 + (y'-y)^2)

runBehavior :: Behavior a -> StdGen -> Plane -> Agent -> (a,Agent,StdGen)
runBehavior action gen plane agent = (a,agent',gen')
  where
    agentM' = runStateT (runReaderT action plane) agent
    ((a,agent'),gen') = runRand agentM' gen

-- | Constructs a sensor config
mkSensor :: Int     -- ^ Number of scans per ray
         -> Int     -- ^ Number of rays
         -> Float   -- ^ Ray length
         -> Float   -- ^ Radius of a scan point
         -> Sensor
mkSensor s r l r'= Sensor l ((2*pi) / toEnum r) (l / toEnum s) r'

-- | Constructs an agent
mkAgent :: Point  -- ^ Starting position
        -> Float  -- ^ Radius of the agent (size)
        -> Vector -- ^ Initial direction
        -> Float  -- ^ Initial speed
        -> Sensor -- ^ Sensor config
        -> Agent
mkAgent p r d s f = Agent p r d s f False False


--------------------------------------------------------------------------------
-- Movement --------------------------------------------------------------------
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Scanning --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Scans the plane around an agent, returning all agents and obstacles withing it's sensor range
scan :: Behavior ([Agent],[Obstacle])
scan = do
  range <- use $ agentSensor . sensorRange
  (,) <$> neighbours range <*> nearObstacles range

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



--------------------------------------------------------------------------------
-- Simulation ------------------------------------------------------------------
--------------------------------------------------------------------------------

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



--------------------------------------------------------------------------------
-- Rendering -------------------------------------------------------------------
--------------------------------------------------------------------------------

class Render a where
  render :: Plane -> a -> Picture

instance Render Agent where
  render _ a = pictures [a', dir]
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

    Sensor r _ _ pr = _agentSensor a

    cross = pictures [line [(-pr,0),(pr,0)],line [(0,-pr),(0,pr)]]

    dir = a
        & _agentDirection
        & (\d@(x,y) -> pictures [line [(0,0)
                                      , (r + 10) `mulSV` normalizeV d]
                                , translate x y cross])
        & color green
        & translate (a^.agentPosition._1) (a^.agentPosition._2)



instance Render Obstacle where
  render _ o = translate (o^.obstPosition._1) (o^.obstPosition._2)
             $ color black
             $ circleSolid (_obstRadius o)

instance Render Plane where
  render _ p = pictures
    $ map (render p) (p^.planeAgents) 
    ++ map (render p) (p^.planeObstacles)
    ++ [circle 100
        & translate 100 100
        & color blue
        ]




