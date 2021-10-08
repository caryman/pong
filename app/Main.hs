{-# LANGUAGE DisambiguateRecordFields #-}

import           Control.Concurrent ( threadDelay )
import           Control.Monad.Morph
import           Control.Monad.State
import           Control.Lens
import           UI.NCurses
import           Pong
import           System.IO
import           Data.Char

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
    pong <- get
    let xLim = fromIntegral $ view playFieldWidth pong
        yLim = fromIntegral $ view playFieldHeight pong
    if k == Quit then lift quit
       else do
            case k of 
                 Slower -> do
                     let currentSpeed = view playFieldSpeed pong
                         newSpeed = currentSpeed + 1000
                     playFieldSpeed .= newSpeed
                 Faster -> do
                     let currentSpeed = view playFieldSpeed pong
                         newSpeed = currentSpeed - 1000
                     playFieldSpeed .= newSpeed
                 RPaddleUp ->
                     rightPaddleY -= movePaddle RPaddleUp pong 1
                 RPaddleDn ->
                     rightPaddleY += movePaddle RPaddleDn pong (yLim-6)
                 LPaddleUp ->
                     leftPaddleY -= movePaddle LPaddleUp pong 1
                 LPaddleDn ->
                     leftPaddleY += movePaddle LPaddleDn pong (yLim-6) 
                 _ -> playFieldSpeed .= view playFieldSpeed pong  

            hoist generalize $ checkBounds ((0, xLim - 1), (0, yLim - 1))

            nextPong <- get
            let x  = view ballX pong
                y  = view ballY pong
                x' = view ballX nextPong
                y' = view ballY nextPong

            lift $ updateDisplay (x, y) (x', y') pong nextPong
            liftIO $ threadDelay $ view playFieldSpeed pong

            loop


movePaddle :: KeyAction -> Pong -> Int -> Int
movePaddle k p limit = do
    let yR = view rightPaddleY p
        yL = view leftPaddleY p
    case k of
        RPaddleUp -> if yR < limit then 0 else 1
        RPaddleDn -> if yR > limit then 0 else 1
        LPaddleUp -> if yL < limit then 0 else 1
        LPaddleDn -> if yL > limit then 0 else 1
        _ -> 0  -- ??


checkKeyboard :: Curses KeyAction
checkKeyboard = do
    w <- defaultWindow
    e <- getEvent w (Just 0)
    return $ case e of
       Nothing -> NoAction
       Just (EventCharacter 'q') -> Quit
       Just (EventCharacter 'r') -> Restart
       Just (EventCharacter 'k') -> RPaddleUp
       Just (EventCharacter 'l') -> RPaddleDn
       Just (EventCharacter 'a') -> LPaddleUp
       Just (EventCharacter 's') -> LPaddleDn
       Just (EventCharacter 'p') -> Pause
       Just (EventCharacter '-') -> Slower
       Just (EventCharacter '=') -> Faster
       Just _ -> NoAction


updateDisplay :: (Int, Int) -> (Int, Int) -> Pong -> Pong -> Curses ()
updateDisplay (x, y) (x', y') p p' = do
    let pLX = view leftPaddleX p
        pRX = view rightPaddleX p
        pLX' = view leftPaddleX p'
        pRX' = view rightPaddleX p'
        pLY = view leftPaddleY p
        pRY = view rightPaddleY p
        pLY' = view leftPaddleY p'
        pRY' = view rightPaddleY p'
    ballColor <- newColorID ColorWhite ColorBlack 1
    paddleColor <- newColorID ColorGreen ColorBlack 2
    blankColor <- newColorID ColorBlack ColorBlack 3
    ballPosColor <- newColorID ColorBlue ColorWhite 4
    paddlePosColor <- newColorID ColorRed ColorWhite 5
    w <- defaultWindow
    updateWindow w $ do
        clear
        setColor ballPosColor
        moveCursor 0 40
        drawString (show x' ++ " " ++ show y')
        setColor paddlePosColor
        moveCursor 0 2
        drawString (show pLX' ++ " " ++ show pLY')
        moveCursor 0 75
        drawString (show pRX' ++ " " ++ show pRY')
        setColor ballColor
        moveCursor (fromIntegral y') (fromIntegral x')
        drawString "o"
        moveCursor (fromIntegral y) (fromIntegral x)
        drawString " "
        -- drawBlock ((fromIntegral pRX), (fromIntegral pRY)) blankColor   -- erase right paddle
        -- drawBlock ((fromIntegral pLX), (fromIntegral pLY)) blankColor   -- erase left paddle
        drawBlock ((fromIntegral pRX'), (fromIntegral pRY')) paddleColor  -- draw right paddle
        drawBlock ((fromIntegral pLX'), (fromIntegral pLY')) paddleColor  -- draw left paddle
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
                    , _speed          = 50000
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
    paddleColor <- newColorID ColorGreen ColorBlack 6
    w <- defaultWindow
    (rows, cols) <- updateWindow w $ do
        clear
        drawBlock (78, 5) paddleColor  -- draw right paddle
        drawBlock (2, 5) paddleColor  -- draw left paddle
        windowSize
    render
    return (rows, cols)

drawBlock :: (Integer, Integer) -> ColorID -> Update ()
drawBlock (x, y) paddleColor = do
    setColor paddleColor
    moveCursor y (x-1)
    drawGlyph glyphCornerUL
    moveCursor (y+1) (x-1)
    drawGlyph glyphLineV
    moveCursor (y+2) (x-1)
    drawGlyph glyphLineV
    moveCursor (y+3) (x-1)
    drawGlyph glyphLineV
    moveCursor (y+4) (x-1)
    drawGlyph glyphCornerLL

    moveCursor y x
    drawGlyph glyphCornerUR
    moveCursor (y+1) x
    drawGlyph glyphLineV
    moveCursor (y+2) x
    drawGlyph glyphLineV
    moveCursor (y+3) x
    drawGlyph glyphLineV
    moveCursor (y+4) x
    drawGlyph glyphCornerLR
   
main :: IO ()
main = do
    void . runCurses . flip runStateT initialState $ do
        (rows, cols) <- lift initialize
        playFieldHeight .= rows
        playFieldWidth .= cols

        -- set initial paddle positions based on the window dimensions
        pong <- get
        leftPaddleX  .= 2 -- padding
        leftPaddleY  .= 5 -- (fromIntegral rows - (view leftPaddleHeight pong)) `div` 2
        rightPaddleX .= 78 -- (fromIntegral cols - padding)
        rightPaddleY .= 5 -- (fromIntegral rows - (view rightPaddleHeight pong)) `div` 2

        loop
    where
        padding = 2

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
