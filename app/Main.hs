{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent ( threadDelay )
import           Control.Concurrent.MVar
import           Control.Monad.Loops
import           Control.Monad.State
import           Control.Applicative
import           Control.Lens
import           UI.NCurses
import           Pong
import           Config
import           System.IO
import           System.Directory
import           Data.Char

{- ===========================
   boundary function checks boundary conditions
   low: lower limit
   hi:  high limit
   pos: current position
   inc: current increment
   returns: adjusted (pos, inc)
   =========================== -}
boundary :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
boundary (low, hi) (pos, inc)
   | (pos == low && inc < 0) || (pos == hi && inc > 0) = (pos - inc, -inc)
   | otherwise = (pos + inc, inc)

checkBounds :: ((Integer, Integer), (Integer, Integer)) -> Pong ()
checkBounds (xLim, yLim) = do
    p <- get
    let (x, y)   = p ^. ball . position
        (vx, vy) = p ^. ball . velocity
        (x', dx') = boundary xLim (x, vx)
        (y', dy') = boundary yLim (y, vy)

    (score . _1) += if x == (snd xLim) then 1 else 0
    (score . _2) += if x == (fst xLim) then 1 else 0

    (ball . position) .= (x', y')
    (ball . velocity) .= (dx', dy')

processInput :: KeyAction -> Pong ()
processInput k = do
    pong <- get
    let xLim = pong ^. playFieldSpecs . size . width
        yLim = pong ^. playFieldSpecs . size . height

    case k of
        Slower ->
            speed += 1000
        Faster -> do
            speed -= 1000
        RPaddleUp ->
            (rightPaddle . position . _y) -= movePaddle RPaddleUp pong 1
        RPaddleDn ->
            (rightPaddle . position . _y) += movePaddle RPaddleDn pong (yLim-6)
        LPaddleUp ->
            (leftPaddle . position . _y)  -= movePaddle LPaddleUp pong 1
        LPaddleDn ->
            (leftPaddle . position . _y)  += movePaddle LPaddleDn pong (yLim-6)
        _ -> return ()

    checkBounds ((0, xLim - 1), (0, yLim - 1))
    nextPong <- get
    lift $ updateDisplay (pong ^. ball . position) (nextPong ^. ball . position) pong nextPong


movePaddle :: KeyAction -> PongState -> Integer -> Integer
movePaddle k p limit = do
    let yR = p ^. rightPaddle . position . _y
        yL = p ^. leftPaddle . position . _y
    case k of
        RPaddleUp -> if yR < limit then 0 else 1
        RPaddleDn -> if yR > limit then 0 else 1
        LPaddleUp -> if yL < limit then 0 else 1
        LPaddleDn -> if yL > limit then 0 else 1
        _ -> 0  -- ??


checkKeyboard :: Curses (Maybe KeyAction)
checkKeyboard = do
    w <- defaultWindow
    e <- getEvent w (Just 0)
    return $ case e of
       Nothing -> Just NoAction
       Just (EventCharacter 'q') -> Nothing
       Just (EventCharacter 'r') -> Just Restart
       Just (EventCharacter 'k') -> Just RPaddleUp
       Just (EventCharacter 'l') -> Just RPaddleDn
       Just (EventCharacter 'a') -> Just LPaddleUp
       Just (EventCharacter 's') -> Just LPaddleDn
       Just (EventCharacter 'p') -> Just Pause
       Just (EventCharacter '-') -> Just Slower
       Just (EventCharacter '=') -> Just Faster
       Just _ -> Just NoAction


updateDisplay :: (Integer, Integer) -> (Integer, Integer) -> PongState -> PongState -> Curses ()
updateDisplay (x, y) (x', y') p p' = do
    let (pLX, pLY)   = p  ^. leftPaddle  . position
        (pRX, pRY)   = p  ^. rightPaddle . position
        (pLX', pLY') = p' ^. leftPaddle  . position
        (pRX', pRY') = p' ^. rightPaddle . position
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
        -- drawString (show x' ++ " " ++ show y')
        drawString (show (p' ^. score . _1) ++ "-" ++ show (p ^. score . _2))
        setColor paddlePosColor
        moveCursor 0 2
        drawString (show pLX' ++ " " ++ show pLY')
        moveCursor 0 75
        drawString (show pRX' ++ " " ++ show pRY')
        setColor ballColor
        moveCursor y' x'
        drawString "o"
        -- moveCursor y x
        -- drawString " "
        -- drawBlock (pRX, pRY) blankColor   -- erase right paddle
        -- drawBlock (pLX, pLY) blankColor   -- erase left paddle
        drawBlock (pRX', pRY') paddleColor  -- draw right paddle
        drawBlock (pLX', pLY') paddleColor  -- draw left paddle
    render
    liftIO $ threadDelay $ p ^. speed

initialState :: PongState
initialState = PongState
    { _ball           = initialBall
    , _paddles        = (leftPaddle, rightPaddle)
    , _playFieldSpecs = playFieldSpecs
    , _score          = (0, 0)
    , _speed          = 50000
    }
    where
        initialBall = Ball
            { _position = (1, 1)
            , _velocity = (1, 1)
            }
        leftPaddle = Paddle
            { _position = (1, 5)
            , _height   = 6
            }
        rightPaddle = Paddle
            { _position = (80, 5)
            , _height   = 6
            }
        fieldSize = PlayFieldSize
            { _width      = 80
            , _height     = 25
            , _resolution = 60  -- ?
            }
        fieldColors = Colors
            { _background = ColorRGB 0 0 0
            , _foreground = ColorRGB 255 255 255
            }
        playFieldSpecs = PlayFieldSpecs
            { _size  = fieldSize
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

readConfig :: IO ()
readConfig = do
    path <- System.Directory.getCurrentDirectory
    let fileNameAndPath = path ++ "/" ++ fileName
    print(fileNameAndPath)
    ymlData <- readConfigurationGeneric fileNameAndPath :: IO PongGameConfig
    print $ ymlData
    writeConfiguration fileNameAndPath ymlData
    newConfig <- newConfiguration fileNameAndPath :: IO Configuration
    print $ configurationPath newConfig
    mv <- takeMVar (configurationVar newConfig)
    print $ mv

main :: IO ()
main = do
    readConfig
    void . runCurses . flip runStateT initialState $ do
        pong <- get
        (rows, cols) <- lift initialize
        (playFieldSpecs . size . height) .= rows
        (playFieldSpecs . size . width)  .= cols

        -- set initial paddle positions based on the window dimensions
        (leftPaddle . position . _x)  .= 2 -- padding
        (leftPaddle . position . _y)  .= 5 -- (rows - (pong ^. leftPaddleHeight)) `div` 2
        (rightPaddle . position . _x) .= 78 -- (cols - padding)
        (rightPaddle . position . _y) .= 5 -- (rows - (pong ^. rightPaddleHeight)) `div` 2

        whileJust_ (lift checkKeyboard) processInput
        lift quit
    where
        padding = 2

quit :: Curses ()
quit = do
    w <- defaultWindow
    updateWindow w $ do
        clear
    render

