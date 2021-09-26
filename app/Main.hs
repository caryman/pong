{-# LANGUAGE DisambiguateRecordFields #-}

import           Control.Concurrent ( threadDelay )
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Lens
import           UI.NCurses
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


loop :: StateT Pong IO ()
loop = do
    pong <- get

    --w <- defaultWindow
    --ev <- getEvent w (Just 0)
    --case ev of
    --    EventCharacter 'q' -> quit

    let xLim = view playFieldWidth pong
        yLim = view playFieldHeight pong
    hoist generalize $ checkBounds ((1, xLim), (1, yLim))
    nextPong <- get
    let x  = view ballX pong
        y  = view ballY pong
        x' = view ballX nextPong
        y' = view ballY nextPong

    liftIO $ updateDisplay (x, y) (x', y')
    loop


updateDisplay :: (Int, Int) -> (Int, Int) -> IO ()
updateDisplay (x, y) (x', y') = do
    move x' y'
    draw "o"
    erase x y
    threadDelay 50000


state' :: ((Int, Int), (Int, Int)) -> Pong -> Pong
state' = execState . checkBounds

states :: ((Int, Int), (Int, Int)) -> Pong -> [Pong]
states limits initState = iterate (state' limits) initState
--states limits = iterate (state' limits) --remove initState, point free style

initialize :: IO ()
initialize = do
    cls
    hSetBuffering stdout NoBuffering
    setEcho False
    putStr "\ESC[?25l" -- hide cursor - replace with ncurses cmd
    let initialBall  = Ball { _position = Point 1 1
                            , _velocity = Velocity 1 1
                            }
        leftPaddle   = Paddle { _position = Point 1 5
                              , _height   = 6
                              }
        rightPaddle  = Paddle { _position = Point 80 5
                              , _height   = 6
                              }
        fieldSize = PlayFieldSize { _width      = 80
                                  , _height     = 25
                                  , _resolution = 60  -- ?
                                  }
        fieldColors = Colors { _background = ColorRGB 0 0 0
                             , _foreground = ColorRGB 255 255 255
                             }
        playFieldSpecs = PlayFieldSpecs { _size  = fieldSize
                                        , _color = fieldColors
                                        }
        initialState = Pong { _ball           = initialBall
                            , _paddles        = (leftPaddle, rightPaddle)
                            , _playFieldSpecs = playFieldSpecs
                            , _score          = (0, 0)
                            }
    --putStrLn $ show (take 10 (states ((1,80),(1,25)) initialState))
    evalStateT loop initialState

main :: IO ()
main = runCurses $ initialize

quit :: IO ()
quit = cls
    

--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
