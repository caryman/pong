# pong
The initial phase of this project is to get the ball bouncing on the screen (in Haskell), in one direction (x), then both directions (x,y). This has been accomplished in several ways:

1. using function pattern matching - very messy
2. using guards - better
3. using where clause

For the where clause, we initially broke out the boundary condition checking code into a separate function using function pattern matching. Then we simplified the code by changing the function definition to use tuples to pass the ball position and increment for x and y coordinates. Further simplifications provided for static declarations of boundary limits.

To do: (not in any particular order)

1. Add paddles (requires check for keyboard events)
2. Add acceleration and variable ball velocity
3. Add checks for above
4. Add more balls
5. Separate out impure from pure code
6. Break out functions into separate threads
7. Check for keyboard activity in its own thread
8. Add obstructions
9. Add more windows
10. Allow balls to pass between windows
11. Randomize starting ball position
12. Add degree of difficulty
13. Add option to change default speed (of ball)
14. Add acceleration, momentum, weight?, size to ball
15. Add responsivity to paddles
16. Add score board
17. Add gaming structure - start, end, save, game tally, player 1/2
18. Add timer
19. Add moving targets
20. Add moving obstructions
21. Add option to change size of paddles
