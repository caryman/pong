import Control.Concurrent ( threadDelay )
import Lib
import System.IO

paint :: String -> IO ()
paint a = do
    putStr a
    hFlush stdout

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

loop :: (Int, Int, Int, Int) -> IO ()
loop (x, dx, y, dy) = do
              move x' y'
              draw "o"
              erase x y
              hFlush stdout
              threadDelay 50000
              loop (x', dx', y', dy')  --already printed this position
   where (x', dx') = boundary (1, 80) (x, dx) 
         (y', dy') = boundary (1, 25) (y, dy)  

main :: IO ()
main = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    move 1 1
    draw "X"
    dostuff (1, 1, 1, 1)  


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
