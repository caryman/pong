import           Control.Concurrent ( threadDelay )
import           Control.Lens
import           Lib
import           Pong
import           System.IO

hiLimit = 80
loLimit = 1

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

loop :: ((Int, Int), (Int, Int)) -> Pong -> IO ()
loop (xLim, yLim) pong = do
              move x' y'
              draw "o"
              erase (pong ^. ball ^. position ^. x) (pong ^. ball ^. position ^. y)
              hFlush stdout
              threadDelay 50000
              loop (xLim, yLim) nextState  --already printed this position
   where (x', dx') = boundary xLim (pong ^. ball ^. position ^. x, pong ^. ball ^. velocity ^. dx)
         (y', dy') = boundary yLim (pong ^. ball ^. position ^. y, pong ^. ball ^. velocity ^. dy)
         nextBall = Ball (Point x' y') (Velocity dx' dy')
         nextPaddles = pong ^. paddles
         nextState = Pong nextBall nextPaddles

state' :: ((Int, Int), (Int, Int)) -> Pong -> Pong
state' (xLim, yLim) pong = nextState
   where (x', dx') = boundary xLim (pong ^. ball ^. position ^. x, pong ^. ball ^. velocity ^. dx)
         (y', dy') = boundary yLim (pong ^. ball ^. position ^. y, pong ^. ball ^. velocity ^. dy)
         nextBall = Ball (Point x' y') (Velocity dx' dy')
         nextPaddles = pong ^. paddles
         nextState = Pong nextBall nextPaddles


states :: ((Int, Int), (Int, Int)) -> Pong -> [Pong]
states limits initState = iterate (state' limits) initState
--states limits = iterate (state' limits) --remove initState, point free style

initialize :: IO ()
initialize = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    --move 1 1
    --draw "X"
    let initialBall = Ball (Point 1 1) (Velocity 1 1)
    let leftPaddle = Paddle (Point 1 30) 30
    let rightPaddle = Paddle (Point 80 30) 30
    let initialState = Pong initialBall (leftPaddle, rightPaddle)
    putStrLn $ show (take 10 (states ((1,80),(1,25)) initialState))
    --loop ((1,80),(1,25)) initialState

main :: IO ()
main = initialize


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
