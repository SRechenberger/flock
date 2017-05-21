{-# LANGUAGE TemplateHaskell #-}
module Simulation
  (
  -- * Simulation
  runSim
  )where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

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
  a = mkAgent p 3 (1,1) 1 (mkSensor 8 8 32 8)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) =
  pause %~ not
inputHandler (EventKey (Char 'r') Down _ _) =
  plane .~ Plane [] []
inputHandler _ = id

-- | One step of the simulation, unsing the @step@ function.
simStep :: (Plane -> Agent -> Agent) -> Float -> Transition
simStep f _ p
  | p^.pause = p
  | otherwise = p & plane %~ step f

-- | As @simStep@, but with its behavior function in a random monad.
simStepR :: (Plane -> Agent -> Rand StdGen Agent) -> Float -> Transition
simStepR f _ p
  | p^.pause  = p
  | otherwise = p{ _plane=plane', _rgen=gen'}
 where
  (plane', gen') = runRand (stepR f (_plane p)) (_rgen p)


-- | Runs the simulation in a window, given the window size,
--   a plane, to simulate and a behavior function.
runSim :: Int -> Int -> Plane
       -> Behavior ()
       -> IO ()
runSim w h p b =
  runSimR w h p
    (\pl ag -> do
      ((),a') <- runStateT (runReaderT b pl) ag
      return a')

-- | As @runSim@ but with a random-monadic behavior function.
runSimR :: Int                         -- ^ Window width
        -> Int                         -- ^ Window height
        -> Plane                       -- ^ Plane to simulate
        -> (Plane -> Agent -> Rand StdGen Agent)   -- ^ Behavior function
        -> IO ()
runSimR w h p f = do
  r <- getStdGen
  play
    (window w h)
    bgColor
    fps
    (SimState p r True)
    (render p . _plane)
    inputHandler
    (simStepR f)
