{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent ( threadDelay )
import Control.Lens
import Lib
import System.IO

hiLimit = 80
loLimit = 1

data Point = Point { _x :: Int
                   , _y :: Int
                   } deriving (Eq, Show)

data Velocity = Velocity { _dx :: Int
                         , _dy :: Int
                         } deriving (Eq, Show)

data Ball = Ball { _position :: Point
                 , _velocity :: Velocity
                 } deriving (Eq, Show)

makeLenses ''Point
makeLenses ''Velocity
makeLenses ''Ball

{- ===========================
   boundary function checks boundary conditions
   low: lower limit
   hi:  high limit
   pos: current position
   inc: current increment
   returns: adjusted (pos, inc)
   =========================== -}
boundary :: (Int, Int) -> (Int, Int) -> (Int, Int)
boundary (low, hi) (pos, inc)
   | (pos == low && inc < 0) || (pos == hi && inc > 0) = (pos - inc, -inc)
   | otherwise = (pos + inc, inc)

loop :: ((Int, Int), (Int, Int)) -> Ball -> IO ()
loop (xLim, yLim) ball = do
              move x' y'
              draw "o"
              erase (ball^.position^.x) (ball^.position^.y)
              hFlush stdout
              threadDelay 50000
              loop (xLim, yLim) nextBall  --already printed this position
   where (x', dx') = boundary xLim (ball^.position^.x, ball^.velocity^.dx)
         (y', dy') = boundary yLim (ball^.position^.y, ball^.velocity^.dy)
         nextBall = Ball (Point x' y') (Velocity dx' dy')

state' :: ((Int, Int), (Int, Int)) -> Ball -> Ball
state' (xLim, yLim) ball = nextBall
   where (x', dx') = boundary xLim (ball^.position^.x, ball^.velocity^.dx)
         (y', dy') = boundary yLim (ball^.position^.y, ball^.velocity^.dy)
         nextBall = Ball (Point x' y') (Velocity dx' dy')

states :: ((Int, Int), (Int, Int)) -> Ball -> [Ball]
states limits initState = iterate (state' limits) initState
--states limits = iterate (state' limits) --remove initState, point free style

initialize :: IO ()
initialize = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    --move 1 1
    --draw "X"
    let initialState = Ball (Point 1 1) (Velocity 1 1)
    putStrLn (show (take 10 (states ((1,80),(1,25)) initialState)))
    --loop ((1,80),(1,25)) initialState

main :: IO ()
main = initialize


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
