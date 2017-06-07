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
import Data.Maybe (isJust)

main :: IO ()
main = runSim 1000 1000 0 area behavior

area :: Plane
area = Plane
  { _planeAgents =
    [ mkAgent (i,j) rad (0,0) 1 sensorRange
    | i <- [-100, -90..100]
    , j <- [-100, -90..100]
    , j^2 + i^2 <= 100 ^2
    ]
  , _planeObstacles = []
  }

rad, sensorRange, desired, minimal :: Float
rad = 5
sensorRange = rad * 10
desired = rad * 1.1
minimal = sensorRange

collisionAvoidance :: Behavior Bool
collisionAvoidance = do
  (_, os) <- scanD 0 (pi/4)
  self <- get
  let
    cos = os
      & find (\o ->
        distance self o <= rad * 1.5)
      & isJust
  when cos $ do
    lr <- uniform [-pi/2,pi/2]
    turn lr
  return cos

separation :: Behavior Bool
separation = do
  (as, _) <- scanD 0 (pi*3/4)
  self <- get
  let
    cas =
      find
        (\a ->
          distance self a <= rad * 1.5)
        as
  case cas of
    Nothing -> do
      return False
    Just a  -> do
      when (isOnLeft self a)
        turnRight
      when (isOnRight self a)
        turnLeft
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

  move
  if length (filter _agentFlocking as) >= 1 || length as >= 5
    then agentFlocking .= True
    else agentFlocking .= False

  ca <- collisionAvoidance
  when (not ca) $ do
    s <- separation
    when (not s) $ do
      c <- cohesion
      when (not c)
        move
