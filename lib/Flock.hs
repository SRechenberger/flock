{-# LANGUAGE TemplateHaskell #-}
module Flock
  ( 
  -- * Behavior Monad
    Behavior
  , runBehavior
  -- ** Type
  , Agent (..)
  -- ** Constructor
  , mkAgent
  -- ** Control functions
  , move, scan, turn, turnTowards, turnAway --, neighbours, nearObstacles
  -- ** Lenses
  , agentPosition, agentRadius, agentDirection, agentSpeed
  , agentSensorRange, agentFlocking, agentAtDest
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
  -- * Utils
  , dist, collidesWith, distance, angle
  , isOnLeft, isOnRight, scanD, lookThere
  , IsObject (..)
  )
  where

import Control.Lens
  ( makeLenses
  , (^.), (&), use, (%~), _1, _2, both, view, (%=), (.=))

import Control.Monad.Random (Rand, runRand, getRandomR)

import Control.Monad.Reader

import Control.Monad.State

import Data.List (find)

import Graphics.Gloss.Data.Picture -- (Point)
import Graphics.Gloss.Data.Vector -- (Vector, normalizeV)
import Graphics.Gloss.Data.Color

import System.Random (StdGen)

import Debug.Trace (trace)

--------------------------------------------------------------------------------
-- Data ------------------------------------------------------------------------
--------------------------------------------------------------------------------

type Angle = Float
type Distance = Float

-- | Agents are circular autonomous mobile objects, searching of there destination.
data Agent = Agent
  { _agentPosition    :: Point   -- ^ Position of the agent on the plane
  , _agentRadius      :: Float   -- ^ Radius of the agent
  , _agentDirection   :: Vector  -- ^ Direction, in which the agent is heading
  , _agentSpeed       :: Float   -- ^ Speed at which the agent is moving
  , _agentSensorRange :: Distance-- ^ Sensor range of the agent
  , _agentFlocking    :: Bool    -- ^ Is the agent in a flock?
  , _agentAtDest      :: Bool    -- ^ Is the agent or its flock at its destination?
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

makeLenses ''Agent
makeLenses ''Obstacle
makeLenses ''Plane


runBehavior :: Behavior a -> StdGen -> Plane -> Agent -> (a,Agent,StdGen)
runBehavior action gen plane agent = (a,agent',gen')
  where
    agentM' = runStateT (runReaderT action plane) agent
    ((a,agent'),gen') = runRand agentM' gen

-- | Constructs an agent
mkAgent :: Point  -- ^ Starting position
        -> Float  -- ^ Radius of the agent (size)
        -> Vector -- ^ Initial direction
        -> Float  -- ^ Initial speed
        -> Distance -- ^ Sensor range
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

-- | Moves an agent, according to its state;
move :: Behavior ()
move = do
  (dx,dy) <- use agentDirection
  when (dx == 0 && dy == 0) $ do
    d <- (,) <$> getRandomR (-1,1) <*> getRandomR (-1,1)
    agentDirection .= d
  modify move'




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
turnTowards' t agent = agent { _agentDirection = d' }
 where
  p = _agentPosition agent
  d' = t-p

-- | Lets the agent directly head to a specific point.
turnTowards :: Point -> Behavior ()
turnTowards = modify . turnTowards'

turnAway :: Point -> Behavior ()
turnAway p = do
  turnTowards p
  agentDirection %= negate

--------------------------------------------------------------------------------
-- Scanning --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Scans the plane around an agent, returning all agents and obstacles withing it's sensor range
scan :: Behavior ([Agent],[Obstacle])
scan = do
  self <- get
  range <- use $ agentSensorRange
  agents <- filter (/= self)
          . filter ((<= range) . distance self)
          <$> view planeAgents
  obsts <- filter ((<= range) . distance self)
         <$> view planeObstacles
  return (agents, obsts)


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

-- | Resolves collisions
bump :: Plane -> Plane
bump (Plane as os) = Plane as' os
 where
  as' = bump' as
  bump' as = case find (\(a,o) -> a `collidesWith` o) [ (a,o) | a <- as, o <- os] of
    Nothing    -> as
    Just (a,o) -> bump' $ map
      (\a' -> if position a == position a'
        then let
            d = max (radius a) $ dist (position o) (position a)
            v' = position o - position a
            v | v' == (0,0) = (1,1)
              | otherwise   = v'
            m = trace (show d) $ 1/magV v
            off = d `mulSV` ((m,m) * v)
          in trace (show off) a { _agentPosition = position a + off }
        else a')
      as





--------------------------------------------------------------------------------
-- Collision -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A class of objects in the simulated plane.
class IsObject obj where
  -- | Returns the radius of an object.
  radius :: obj -> Float
  -- | Returns the position of an object.
  position :: obj -> Point

instance IsObject Obstacle where
  radius = _obstRadius
  position = _obstPosition

instance IsObject Agent where
  radius = _agentRadius
  position = _agentPosition

-- | Calculates the edge-to-edge-distance between two objects.
distance :: (IsObject a, IsObject b)
         => a -> b -> Distance
distance a b = dist (position a) (position b) - (radius a + radius b)

dist :: Point -> Point -> Float
dist (x,y) (x',y') = sqrt ((x'-x)^2 + (y'-y)^2)

-- | Checks, whether two objects collide.
collidesWith :: (IsObject a, IsObject b)
             => a -> b -> Bool
a `collidesWith` b = distance a b <= 0

-- | Returns the Angle between the direction vector
--   and the vector between the agent and another object;
--
--   ATTENTION! The angle returned is symmetrical for both sides;
--   i.e.: if the angle is, e.g., PI/2, it is either 90 Degrees on the left
--   or on the right side.
angle :: (IsObject a)
      => Agent -> a -> Angle
angle a o = angleVV (position o - position a) (_agentDirection a)

lookThere :: (IsObject a)
          => Angle -> Angle -> Agent -> a -> Bool
lookThere ga be a o = al <= be
 where
  al = angleVV dl dp
  dl = rotateV ga (_agentDirection a)
  dp = position o - position a

-- | Checks, whether a give object is on the @right@ side of the agent.
isOnRight :: (IsObject a)
         => Agent -> a -> Bool
isOnRight = lookThere (pi/2) (pi/2)

-- | Checks, whether a give object is on the @left@ side of the agent.
isOnLeft :: (IsObject a)
         => Agent -> a -> Bool
isOnLeft = lookThere (-pi/2) (pi/2)

-- | Checks, whether a give object is @behind@ the agent.
isBehind :: (IsObject a)
         => Agent -> a -> Bool
isBehind = lookThere pi (pi/2)

-- | Scans only in a certain direction, with a certain field of view
scanD :: ()
      => Angle  -- ^ Direction of view
      -> Angle  -- ^ Field of view
      -> Behavior ([Agent],[Obstacle])
scanD ga be = do
  scn <- scan
  self <- get
  return $ scn
         & _1 %~ filter (lookThere ga be self)
         & _2 %~ filter (lookThere ga be self)
  
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

    r = _agentSensorRange a

    cross = pictures [line [(-5,0),(5,0)],line [(0,-5),(0,5)]]

    dir = a
        & _agentDirection
        & (\d@(x,y) -> line [(0,0), (r + 10) `mulSV` normalizeV d])
        & color green
        & translate (a^.agentPosition._1) (a^.agentPosition._2)



instance Render Obstacle where
  render _ o = translate (o^.obstPosition._1) (o^.obstPosition._2)
             $ color black
             $ circleSolid (_obstRadius o)

instance Render Plane where
  render _ p = pictures
    $ map (render p) (p^.planeObstacles)
    ++ map (render p) (p^.planeAgents)
    {-
    ++ [circle 100
        & translate 100 100
        & color blue
        ]
    -}




