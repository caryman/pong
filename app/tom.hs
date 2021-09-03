import Control.Concurrent ( threadDelay )
import Lib
import System.IO

paint :: String -> IO ()
paint a = do
    putStr a
    hFlush stdout
               
run :: (Int, Int, Int, Int) -> IO ()
run (x, dx, y, dy)
    | x == 10 = print "FUCK!"
    | ((x == 1) || (x == 20)) && ((y == 1) || (y == 10)) = do
              putStr $ "1(" ++ show x ++ "," ++ show y ++ ")"
              hFlush stdout
              let ddx = (-dx)
              let ddy = (-dy) 
              move (x + ddx) (y + ddy)
              draw "."
              erase x y
              hFlush stdout
              threadDelay 100000
              run (x + ddx, ddx, y + ddy, ddy)  --already printed this position
    | ((x == 1) || (x == 20)) = do
              putStr $ "2(" ++ show x ++ "," ++ show y ++ ")"
              hFlush stdout
              let ddx = (-dx)
              move (x + ddx) (y + dy)
              draw "."
              erase x y
              hFlush stdout
              threadDelay 100000
              run (x + ddx, ddx, y + dy, dy)  --already printed this position
    | ((y == 1) || (y == 10)) = do
              putStr $ "3(" ++ show x ++ "," ++ show y ++ ")"
              hFlush stdout
              let ddy = (-dy) 
              move (x + dx) (y + ddy)
              draw "."
              erase x y
              hFlush stdout
              threadDelay 100000
              run (x + dx, dx, y + ddy, ddy)  --already printed this position
    | otherwise = do
              putStr $ "4(" ++ show x ++ "," ++ show y ++ ")"
              hFlush stdout
              move (x + dx) (y + dy)
              draw "."
              erase x y
              hFlush stdout
              threadDelay 100000
              run (x + dx, dx, y + dy, dy)  --already printed this position
              
main :: IO ()
main = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    move 1 1
    draw "X"
    run (1, 1, 1, 1)  


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor