{-# LANGUAGE TemplateHaskell #-}
module Simulation
  (
  -- * Simulation
  runSim
  )where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector (mulSV)

import System.Random (StdGen, getStdGen)
import Control.Monad.Random (Rand, runRand)

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Flock

data SimState = SimState
  { _plane :: Plane
  , _rgen  :: StdGen
  , _pause :: Bool
  , _steps :: Integer
  , _pauseEvery :: Integer
  , _trail :: [Point]
  } deriving Show

makeLenses ''SimState

type Transition = SimState -> SimState

window :: Int -> Int -> Display
window w h = InWindow "Flock" (w,h) (0,0)

bgColor :: Color
bgColor = white

fps :: Int
fps = 30

-- | Simulation control
inputHandler :: Event -> Transition
inputHandler (EventKey (MouseButton LeftButton) Down _ p) =
  plane.planeObstacles %~ (o:)
 where
  o = Obstacle p 10
inputHandler (EventKey (MouseButton RightButton) Down _ p) =
  plane.planeAgents %~ (a:)
 where
  a = mkAgent p 5 (1,1) 1 80
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) =
  pause %~ not
inputHandler (EventKey (Char 'r') Down _ _) =
  (plane .~ Plane [] []).(trail .~ []).(pause .~ True)
inputHandler _ = id

-- | As @simStep@, but with its behavior function in a random monad.
simStepR :: (Plane -> Agent -> Rand StdGen Agent) -> Float -> Transition
simStepR f _ p
  | p^.pause  = p
  | _pauseEvery p > 1 && _steps p `mod` _pauseEvery p == 0
      = p { _pause = True, _steps = _steps p + 1 }
  | otherwise
      = p { _plane = plane'
          , _rgen  = gen'
          , _steps = _steps p + 1
          , _trail = averageP (map _agentPosition (_planeAgents._plane $ p))
                   : _trail p
          }
 where
  averageP ps = (1/toEnum (length ps)) `mulSV` sum ps
  (plane', gen') = runRand (stepR f (_plane p)) (_rgen p)


-- | Runs the simulation in a window, given the window size,
--   a plane, to simulate and a behavior function.
runSim :: Int -> Int -> Integer -> Plane
       -> Behavior ()
       -> IO ()
runSim w h s p b =
  runSimR w h s p
    (\pl ag -> do
      ((),a') <- runStateT (runReaderT b pl) ag
      return a')

-- | As @runSim@ but with a random-monadic behavior function.
runSimR :: Int                         -- ^ Window width
        -> Int                         -- ^ Window height
        -> Integer                     -- ^ Stop every n steps.
        -> Plane                       -- ^ Plane to simulate
        -> (Plane -> Agent -> Rand StdGen Agent)   -- ^ Behavior function
        -> IO ()
runSimR w h s p f = do
  r <- getStdGen
  play
    (window w h)
    bgColor
    fps
    (SimState p r True 1 s [])
    (renderSim)
    inputHandler
    (simStepR f)

renderSim :: SimState -> Picture
renderSim ss = pictures
  [ render (_plane ss) (_plane ss)
  , line (_trail ss)
  ]
