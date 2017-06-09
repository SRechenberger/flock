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

import System.Environment (getArgs)

import Graphics.Gloss.Data.Vector (rotateV)

main :: IO ()
main = do
  let
    rad = 5
    srange = 30
    desired = 10
    minimal = 2
    area = Plane
      { _planeAgents =
        [ mkAgent (rotateV a (30,0)) rad (0,0) 1 srange
        | a <- [0,pi/8..2*pi]
        ]
      , _planeObstacles = [ Obstacle (rotateV a (200,0)) 10 | a <- [0,pi/32..2*pi]]
      }
  runSim 500 500 (120*60) area (behavior minimal desired)

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
desired = sensorRange * 0.9
minimal = sensorRange * 0.2

collisionAvoidance :: Distance -> Behavior Bool
collisionAvoidance minimum = do
  (_, os) <- scanD 0 (pi/4)
  self <- get
  let
    cos = os
      & find (\o ->
        distance self o <= minimum)
      & isJust
  when cos $ do
    lr <- uniform [-pi/2,pi/2]
    turn lr
  return cos

separation :: Distance -> Behavior Bool
separation minimum = do
  (as, _) <- scanD 0 (pi*3/4)
  self <- get
  let
    cas =
      find
        (\a ->
          distance self a <= minimum)
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

cohesion :: Distance -> Behavior Bool
cohesion desired = do
  self <- get

  let
    p a = distance self a > desired
    f = length . filter p . fst

  ls <- f <$> scanD (-pi/2) (pi/4)
  rs <- f <$> scanD (pi/2) (pi/4)
  bs <- f <$> scanD pi (pi/4)

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

behavior :: Distance -> Distance -> Behavior ()
behavior minimum desired = do
  (as, os) <- scan
  self     <- get

  move
  if length (filter _agentFlocking as) >= 1 || length as >= 5
    then agentFlocking .= True
    else agentFlocking .= False

  ca <- collisionAvoidance minimum
  when (not ca) $ do
    s <- separation minimum
    when (not s) $ do
      c <- cohesion desired
      when (not c)
        move
