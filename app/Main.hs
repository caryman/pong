import           Control.Concurrent ( threadDelay )
import           Control.Monad.State
import           Control.Lens
import           Lib
import           Pong
import           System.IO

hiLimit = 80
loLimit = 1

-- | From https://stackoverflow.com/questions/17325485/combining-statet-io-with-state
hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState


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
    let x = view ballX p
        y = view ballY p
        vx = view ballDx p
        vy = view ballDy p
        (x', dx') = boundary xLim (x, vx)
        (y', dy') = boundary yLim (y, vy)

    ballX .= x'
    ballY .= y'
    ballDx .= dx'
    ballDy .= dy'


loop :: ((Int, Int), (Int, Int)) -> StateT Pong IO ()
loop (xLim, yLim) = do
    pong <- get
    hoistState $ checkBounds (xLim, yLim)
    nextPong <- get
    let x  = view ballX pong
        y  = view ballY pong
        x' = view ballX nextPong
        y' = view ballY nextPong

    liftIO $ updateDisplay (x, y) (x', y')
    loop (xLim, yLim)


updateDisplay :: (Int, Int) -> (Int, Int) -> IO ()
updateDisplay (x, y) (x', y') = do
    move x' y'
    draw "o"
    erase x y
    hFlush stdout
    threadDelay 50000


state' :: ((Int, Int), (Int, Int)) -> Pong -> Pong
state' = execState . checkBounds

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
    let initialBall  = Ball (Point 1 1) (Velocity 1 1)
        leftPaddle   = Paddle (Point 1 30) 30
        rightPaddle  = Paddle (Point 80 30) 30
        initialState = Pong initialBall (leftPaddle, rightPaddle)
    --putStrLn $ show (take 10 (states ((1,80),(1,25)) initialState))
    evalStateT (loop ((1,80),(1,25))) initialState

main :: IO ()
main = initialize


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
