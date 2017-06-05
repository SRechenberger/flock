module Main where

import Data.Function
  (on)

import Data.List
  (minimumBy, sortBy)
import Data.Maybe
  ()

import Control.Lens
  (use, (.=))

import Control.Monad.Random
  (getRandomR, uniform)
-- import System.Random (StdGen)

import Control.Monad
  (when)

import Control.Monad.State (get)

import Graphics.Gloss
-- import Graphics.Gloss.Geometry.Angle (degToRad)
import Graphics.Gloss.Data.Vector (Vector, mulSV)

import Flock
import Simulation

area :: Plane
area = Plane
  { _planeAgents = concat
      [ replicate 1 (mkAgent (i,j) (5) (0,0) 5 40)
      | i <- [-m,-(m-10)..m]
      , j <- [-m,-(m-10)..m]
      ]
  , _planeObstacles = [] -- [Obstacle (3,3) 5]
  }

 where
  m = 200

dest :: Point
dest = (100,100)

toDest :: Point -> Vector
toDest (x,y) = (fst dest - x, snd dest - y)

sumV :: [Vector] -> Vector
sumV = foldr (\(a,b) (a',b') -> (a+a',b+b')) (0,0)

averageV :: [Vector] -> Vector
averageV vs = (1/toEnum (length vs)) `mulSV` sumV vs

average :: (Enum a, Fractional a) => [a] -> a
average as = sum as / toEnum (length as)

between :: Ord a => a -> a -> a -> Bool
between x a b = a <= x && x <= b

behavior :: Behavior ()
behavior = do
  (as,_) <- scan
  me <- get
  let collision = sortBy (compare `on` (distance me)) $ filter (me `collidesWith`) as
  case collision of
    x:xs -> do
      agentSpeed .= 1
      agentFlocking .= True
      agentAtDest .= False
      turnAway (position x)
    [] | between (sum [ 1 | a <- as, distance me a <= radius me]) 6 6 -> do
          agentSpeed .= 0
          agentAtDest .= True
       | (sum [ 1 | a <- as, distance me a <= radius me]) > 6 -> do
          turnAway (averageV (map position [ a | a <- as, distance me a <= radius me]))
          agentSpeed .= 2
          agentAtDest .= False
       | otherwise -> do
          agentFlocking .= False
          agentAtDest .= False
          agentSpeed .= 1
          case as of
            [] -> do
              agentDirection .= (0,0)
            as -> do
              let d = (averageV (map position as))
              let rr = 50
              dd <- (,) <$> getRandomR (-rr,rr) <*> getRandomR (-rr,rr)
              turnTowards (d+dd)

  move

main :: IO ()
main = do
  runSim 1000 1000 100 area behavior
