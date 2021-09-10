import Control.Concurrent ( threadDelay )
import Lib
import System.IO

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

loop :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int) -> IO ()
loop (xLim, yLim) (x, dx, y, dy) = do
              move x' y'
              draw "o"
              erase x y
              hFlush stdout
              threadDelay 50000
              loop (xLim, yLim) (x', dx', y', dy')  --already printed this position
   where (x', dx') = boundary xLim (x, dx) 
         (y', dy') = boundary yLim (y, dy)  

state' :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
state' (xLim, yLim) (x, dx, y, dy) = (x', dx', y', dy') 
   where (x', dx') = boundary xLim (x, dx) 
         (y', dy') = boundary yLim (y, dy)  

states :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
states limits initState = iterate (state' limits) initState
--states limits = iterate (state' limits) --remove initState, point free style

initialize :: IO ()
initialize = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    --move 1 1
    --draw "X"
    putStrLn (show (take 10 (states ((1,80),(1,25)) (1, 1, 1, 1))))
    --loop ((1,80),(1,25)) (1, 1, 1, 1)  

main :: IO ()
main = initialize


--print("\u001B[?25h") // display cursor
--print("\u001B[?25l") -- hide cursor
