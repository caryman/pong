import           Control.Concurrent ( threadDelay )
import           Control.Monad.State
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

checkBounds :: ((Int, Int), (Int, Int)) -> State Pong ()
checkBounds (xLim, yLim) = do
    p <- get
    let px = view ballX p
        py = view ballY p
        vx = view ballDx p
        vy = view ballDy p
        (x', dx') = boundary xLim (px, vx)
        (y', dy') = boundary yLim (py, vy)

    ballX .= x'
    ballY .= y'
    ballDx .= dx'
    ballDy .= dy'


loop :: ((Int, Int), (Int, Int)) -> Pong -> IO ()
loop (xLim, yLim) pong = do
    move x' y'
    draw "o"
    erase (view ballX pong) (view ballY pong)
    hFlush stdout
    threadDelay 50000
    loop (xLim, yLim) nextState  --already printed this position
  where
    nextState = execState (checkBounds (xLim, yLim)) pong
    x' = view ballX nextState
    y' = view ballY nextState


state' :: ((Int, Int), (Int, Int)) -> Pong -> Pong
state' (xLim, yLim) pong = nextState
  where
    nextState = execState (checkBounds (xLim, yLim)) pong

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
    --putStrLn $ show (take 10 (states ((1,80),(1,25)) initialState))
    loop ((1,80),(1,25)) initialState

main :: IO ()
main = initialize


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
