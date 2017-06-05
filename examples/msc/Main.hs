--------------------------------------------------------------------------------
-- Simulation of the flocking algorithm and the agents used, described in
-- "A Minimalist Flocking Algorithm for Swarm Robots" by
-- Christoph Moeslinger, Thomas Schmickl and Karl Crailsheim.
--------------------------------------------------------------------------------

module Main (main) where

import Flock
import Simulation

import Control.Monad (when)
import Control.Monad.Random (uniform, getRandomR)
import Control.Monad.State (get)

import Control.Lens ((&), (.=))

import Data.List (find, maximumBy)
import Data.Function (on)

main :: IO ()
main = runSim 1000 1000 area behavior

area :: Plane
area = Plane
  { _planeAgents =
    [ mkAgent (i,j) rad (0,0) 1 sensorRange
    | i <- [-100, -90..100]
    , j <- [-100, -90..100]
    , j^2 + i^2 <= 100 ^2
    ]
  , _planeObstacles =
    [ Obstacle (i,j) 10
    | i <- [-300,-280..300]
    , j <- [-300,-280..300]
    , i <= -250 || i >= 250 || j <= -250 || j >= 250
    ]
  }

rad, sensorRange, desired, minimal :: Float
rad = 5
sensorRange = rad * 4
desired = rad * 2
minimal = sensorRange * 3/4

collisionAvoidance :: Behavior Bool
collisionAvoidance = do
  (_, os) <- scan
  self <- get
  let cos = os
          & filter (\o -> angle self o <= pi/4)
          & filter (\o -> distance self o <= rad * 1.5)
          & (not . null)
  when cos $ do
    agentFlocking .= False
    lr <- uniform [-pi/2,pi/2]
    turn lr
  return cos

separation :: Behavior Bool
separation = do
  (as, _) <- scan
  self <- get
  let cas = find
              (\a ->
                angle self a <= pi*3/4
                && distance self a <= rad * 1.5)
              as
  case cas of
    Nothing -> do
      return False
    Just a  -> do
      when (isOnLeft self a)
        turnRight
      when (isOnRight self a)
        turnLeft
      agentFlocking .= False
      return True

cohesion :: Behavior Bool
cohesion = do
  ls <- length . fst <$> scanD (-pi/2) (pi/4)
  rs <- length . fst <$> scanD (pi/2) (pi/4)
  bs <- length . fst <$> scanD pi (pi/4)

  if ls+rs+bs <= 0
  then do
    return False
  else do
    snd $ maximumBy (compare `on` fst)
      [ (ls, turnLeft)
      , (rs, turnRight)
      , (bs, turnRandomly)
      ]
    agentFlocking .= True
    return True

turnStrength :: Angle
turnStrength = pi/4

turnLeft :: Behavior ()
turnLeft = turn (-turnStrength)

turnRight :: Behavior ()
turnRight = turn turnStrength

turnRandomly :: Behavior ()
turnRandomly = do
  action <- uniform [turnLeft, turnRight]
  action

behavior :: Behavior ()
behavior = do
  (as, os) <- scan
  self     <- get

  ca <- collisionAvoidance
  when (not ca) $ do
    s <- separation
    when (not s) $ do
      cohesion
      return ()
  move
