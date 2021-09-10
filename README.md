# pong
The initial phase of this project is to get the ball bouncing on the screen (in Haskell), in one direction (x), then both directions (x,y). This has been accomplished in several ways:

1. using function pattern matching - very messy
2. using guards - better
3. using where clause

For the where clause, we initially broke out the boundary condition checking code into a separate function using function pattern matching. Then we simplified the code by changing the function definition to use tuples to pass the ball position and increment for x and y coordinates. Further simplifications provided for static declarations of boundary limits.

To do: (not in any particular order)

●  Add paddles (requires check for keyboard events)

●  Add acceleration and variable ball velocity
●  Add checks for above
●  Add more balls
●  Separate out impure from pure code
●  Break out functions into separate threads
●  Check for keyboard activity in its own thread
●  Add obstructions
●  Add more windows
●  Allow balls to pass between windows
●  Randomize starting ball position
●  Add degree of difficulty
●  Add option to change default speed (of ball)
●  Add acceleration, momentum, weight?, size to ball
●  Add responsivity to paddles
●  Add score board
●  Add gaming structure - start, end, save, game tally, player 1/2
●  Add timer
●  Add moving targets
●  Add moving obstructions
●  Add option to change size of paddles
