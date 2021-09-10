paint :: String -> IO ()
paint a = do
    putStr a
    hFlush stdout

loop :: ((Int, Int), (Int, Int)) (Int, Int, Int, Int) -> IO ()
loop (xLim, yLim) (x, dx, y, dy) = do
              move x' y'
              draw "o"
              erase x y
              hFlush stdout
              threadDelay 50000
              loop (xLim, yLim) (x', dx', y', dy')  --already printed this position
   where (x', dx') = boundary xLim (x, dx) 
         (y', dy') = boundary yLim (y, dy)  


step :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
step (1, -1, y, dy) = (2, 1, y + dy, dy)
step (80, 1, y, dy) = (79, -1, y + dy, dy)
step (x, dx, 1, -1) = (x + dx, dx, 2, 1)
step (x, dx, 25, 1) = (x + dx, dx, 24, -1)
step (x, dx, y, dy) = (x + dx, dx, y + dy, dy)
              
dostuff :: (Int, Int, Int, Int) -> IO ()
dostuff (x, dx, y, dy) = do
              move x' y'
              draw "o"
              erase x y
              hFlush stdout
              threadDelay 50000
              dostuff (x', dx', y', dy')  --already printed this position
   where (x', dx', y', dy') = step (x, dx, y, dy)

main :: IO ()
main = do
    cls
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    move 1 1
    draw "X"
    dostuff (1, 1, 1, 1)  
              
run :: (Int, Int, Int, Int) -> IO ()
run (1, -1, y, dy) = do
              move 2 (y + dy)
              draw "o"
              erase 1 y
              bel
              hFlush stdout
              threadDelay 50000
              run (2, 1, y + dy, dy)  --already printed this position
run (80, 1, y, dy) = do
              move 79 (y + dy)
              draw "o"
              erase 80 y
              bel
              hFlush stdout
              threadDelay 50000
              run (79, -1, y + dy, dy)  --already printed this position
run (x, dx, 1, -1) = do
              move (x + dx) 2
              draw "o"
              erase x 1
              bel
              hFlush stdout
              threadDelay 50000
              run (x + dx, dx, 2, 1)  --already printed this position
run (x, dx, 25, 1) = do
              move (x + dx) 24
              draw "o"
              erase x 25
              bel
              hFlush stdout
              threadDelay 50000
              run (x + dx, dx, 24, -1)  --already printed this position
run (x, dx, y, dy) = do
              move (x + dx) (y + dy)
              draw "o"
              erase x y
              hFlush stdout
              threadDelay 50000
              run (x + dx, dx, y + dy, dy)  --already printed this position
              
