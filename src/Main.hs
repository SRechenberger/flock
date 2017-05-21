module Main where

-- import Data.Function
-- import Data.List
import Data.Maybe (catMaybes)

import Control.Lens ((^.),_1,_2,_3,(.=))
import Control.Monad.Random (getRandomR)
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
  scn <- scan
  case scn of
    [] -> do
      angle <- getRandomR (-pi/8,pi/8)
      turn angle
      agentFlocking .= False
    agts -> do
      case filter ((<=1) . (^._1)) agts of
        (_,angl,_):_ -> turn (pi-angl)
        [] -> case filter ((> 2) . (^._1)) agts of
                [] -> return ()
                as -> turn (average (map (^._2) as))
      agentFlocking .= True
  move

main :: IO ()
main = do
  runSim 1000 1000 area behavior
