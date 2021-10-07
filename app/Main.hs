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
        rPad = view rightPaddle pong
        lPad = view leftPaddle pong
    case k of 
       Quit -> lift quit
       Slower -> do
               let currentSpeed = view playFieldSpeed pong
                   newSpeed = currentSpeed + 1000
               playFieldSpeed .= newSpeed
       Faster -> do
               let currentSpeed = view playFieldSpeed pong
                   newSpeed = currentSpeed - 1000
               playFieldSpeed .= newSpeed
       RPaddleUp ->
               rightPaddleY -= movePaddle RPaddleUp rPad 0
       RPaddleDn ->
               rightPaddleY += movePaddle RPaddleDn rPad yLim
       LPaddleUp ->
               leftPaddleY -= movePaddle LPaddleUp lPad 0
       LPaddleDn ->
               leftPaddleY += movePaddle LPaddleDn lPad yLim 
       _ -> playFieldSpeed .= view playFieldSpeed pong  

    hoist generalize $ checkBounds ((0, xLim - 1), (0, yLim - 1))

    nextPong <- get
    let x  = view ballX pong
        y  = view ballY pong
        x' = view ballX nextPong
        y' = view ballY nextPong
        pL = view leftPaddle pong
        pR = view rightPaddle pong
        pL' = view leftPaddle nextPong
        pR' = view rightPaddle nextPong

    lift $ updateDisplay (x, y) (x', y') (pL, pR) (pL', pR')
    liftIO $ threadDelay $ view playFieldSpeed pong

    loop


movePaddle :: KeyAction -> Paddle -> Int -> Int
movePaddle k p limit = do
    let y = view paddleY p
    case k of
        RPaddleUp -> if y < limit then 0 else y-1
        RPaddleDn -> if y > limit then limit else y+1
        LPaddleUp -> if y < limit then 0 else y-1
        LPaddleDn -> if y > limit then limit else y+1
        _ -> 0  -- ??


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


updateDisplay :: (Int, Int) -> (Int, Int) -> (Paddle, Paddle) -> (Paddle, Paddle) -> Curses ()
updateDisplay (x, y) (x', y') (pL, pR) (pL', pR') = do
    let pLX = view leftPaddleX pL
        pRX = view rightPaddleX pR
        pLX' = view leftPaddleX pL'
        pRX' = view rightPaddleX pR'
        pLY = view leftPaddleY pL
        pRY = view rightPaddleY pR
        pLY' = view leftPaddleY pL'
        pRY' = view rightPaddleY pR'
    w <- defaultWindow
    ballColor <- newColorID ColorWhite ColorBlack 5
    updateWindow w $ do
        setColor ballColor
        moveCursor (fromIntegral x') (fromIntegral x')
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
    w <- defaultWindow
    (rows, cols) <- updateWindow w $ do
        clear
        windowSize
    render
    return (rows, cols)

drawPaddle :: Integer -> Integer -> Curses ()
drawPaddle x y = do
    w <- defaultWindow
    paddleColor <- newColorID ColorGreen ColorBlack 6
    updateWindow w $ do
        --mapM_ drawBlock $ zip (repeat 79) [5..10]
        drawBlock (x, y) paddleColor
    render

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
    -- drawGlyph glyphBlock
   
main :: IO ()
main = do
    void . runCurses . flip runStateT initialState $ do
        (rows, cols) <- lift initialize
        playFieldHeight .= rows
        playFieldWidth .= cols

        -- set initial paddle positions based on the window dimensions
        pong <- get
        leftPaddleX  .= padding
        leftPaddleY  .= (fromIntegral rows - (view leftPaddleHeight pong)) `div` 2
        rightPaddleX .= (fromIntegral cols - padding)
        rightPaddleY .= (fromIntegral rows - (view rightPaddleHeight pong)) `div` 2

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
