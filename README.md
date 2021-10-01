# pong
The initial phase of this project is to get the ball bouncing on the screen (in Haskell), in one direction (x), then both directions (x,y). This has been accomplished in several ways:

1. using function pattern matching - very messy
2. using guards - better
3. using where clause

For the where clause, we initially broke out the boundary condition checking code into a separate function using function pattern matching. Then we simplified the code by changing the function definition to use tuples to pass the ball position and increment for x and y coordinates. Further simplifications provided for static declarations of boundary limits.

The next phase is to separate out the pure code from the IO. The loop code can be modified to use a state generater that creates a static list of projected positions of the ball. Then this list is used as forcing function data to simluate ball trajectory data instead of using the loop to update the ball trajectory. This allows the ball position state to become pure and pump the next known state to the IO to print. 

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


# Setup (optional)
Due to system library dependencies on ncurses and some implementation issues with the haskell-ncurses library,
building on Ubuntu is more reliable than other Linux distributions.

## Prerequisites
* VirtualBox - https://www.virtualbox.org/
* Vagrant - https://www.vagrantup.com/

## Steps
1. In the project directory, run the following to configure and provision a VM with ncurses and Haskell Stack
```
vagrant up
```

2. ssh to the VM

```
vagrant ssh
```

3. Switch to the /vagrant directory which is shared folder between your host machine and the VM

```
cd /vagrant
```

4. Build using Haskell Stack

```
stack build
```

