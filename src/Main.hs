module Main where

-- import Data.Function
-- import Data.List
import Data.Maybe
  ()

import Control.Lens
  ()

import Control.Monad.Random
  ()
-- import System.Random (StdGen)

import Graphics.Gloss
-- import Graphics.Gloss.Geometry.Angle (degToRad)
import Graphics.Gloss.Data.Vector (Vector, mulSV)

import Flock
import Simulation

area :: Plane
area = Plane
  { _planeAgents = []
  , _planeObstacles = [] -- [Obstacle (3,3) 5]
  }

dest :: Point
dest = (100,100)

toDest :: Point -> Vector
toDest (x,y) = (fst dest - x, snd dest - y)

sumV :: [Vector] -> Vector
sumV = foldr (\(a,b) (a',b') -> (a+a',b+b')) (0,0)

averageV :: [Vector] -> Vector
averageV vs = toEnum (length vs) `mulSV` sumV vs

average :: (Enum a, Fractional a) => [a] -> a
average as = sum as / toEnum (length as)

behavior :: Behavior ()
behavior = do
  move

main :: IO ()
main = do
  runSim 1000 1000 area behavior
