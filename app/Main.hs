{-# LANGUAGE DisambiguateRecordFields #-}

import           Control.Concurrent ( threadDelay )
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Lens
import           UI.NCurses
import           Lib
import           Pong
import           System.IO

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


loop :: StateT Pong Curses ()
loop = do
    k <- lift checkKeyboard
    if k == Quit then lift quit
    else do
        --case k of
        --   Slower -> 
        pong <- get
        let xLim = fromIntegral $ view playFieldWidth pong
            yLim = fromIntegral $ view playFieldHeight pong
        hoist generalize $ checkBounds ((0, xLim - 1), (0, yLim - 1))
        nextPong <- get
        let x  = view ballX pong
            y  = view ballY pong
            x' = view ballX nextPong
            y' = view ballY nextPong

        lift $ updateDisplay (x, y) (x', y')
        liftIO $ threadDelay 50000

        loop

checkKeyboard :: Curses KeyAction
checkKeyboard = do
    w <- defaultWindow
    e <- getEvent w (Just 0)
    return $ case e of
       Nothing -> NoAction
       Just (EventCharacter 'q') -> Quit
       Just (EventCharacter 'r') -> Restart
       Just (EventCharacter 'l') -> RPaddleUp
       Just (EventCharacter 'k') -> RPaddleDn
       Just (EventCharacter 'a') -> LPaddleUp
       Just (EventCharacter 's') -> LPaddleDn
       Just (EventCharacter 'p') -> Pause
       Just (EventCharacter '-') -> Slower
       Just (EventCharacter '=') -> Faster
       Just _ -> NoAction

updateDisplay :: (Int, Int) -> (Int, Int) -> Curses ()
updateDisplay (x, y) (x', y') = do
    w <- defaultWindow
    updateWindow w $ do
        moveCursor (fromIntegral y') (fromIntegral x')
        drawString "o"
        moveCursor (fromIntegral y) (fromIntegral x)
        drawString " "
    render


state' :: ((Int, Int), (Int, Int)) -> Pong -> Pong
state' = execState . checkBounds

states :: ((Int, Int), (Int, Int)) -> Pong -> [Pong]
states limits initState = iterate (state' limits) initState
--states limits = iterate (state' limits) --remove initState, point free style

initialState :: Pong
initialState = Pong { _ball           = initialBall
                    , _paddles        = (leftPaddle, rightPaddle)
                    , _playFieldSpecs = playFieldSpecs
                    , _score          = (0, 0)
                    }
  where
    initialBall  = Ball { _position = Point 1 1
                        , _velocity = Velocity 1 1
                        }
    leftPaddle   = Paddle { _position = Point 1 5
                          , _height   = 6
                          }
    rightPaddle  = Paddle { _position = Point 80 5
                          , _height   = 6
                          }
    fieldSize    = PlayFieldSize { _width      = 80
                                 , _height     = 25
                                 , _resolution = 60  -- ?
                                 }
    fieldColors  = Colors { _background = ColorRGB 0 0 0
                          , _foreground = ColorRGB 255 255 255
                          }
    playFieldSpecs = PlayFieldSpecs { _size  = fieldSize
                                    , _color = fieldColors
                                    }

initialize :: Curses (Integer, Integer)
initialize = do
    setCBreak True
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    (rows, cols) <- updateWindow w $ do
        clear
        windowSize
    render
    return (rows, cols)

main :: IO ()
main = do
  void . runCurses . flip runStateT initialState $ do
    (rows, cols) <- lift initialize
    playFieldHeight .= rows
    playFieldWidth .= cols
    loop

quit :: Curses ()
quit = do
    w <- defaultWindow
    updateWindow w $ do
        clear
    render

-- scrap bits
--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
--putStrLn $ show (take 10 (states ((1,80),(1,25)) initialState))

-- ghcup
-- haskell language server
