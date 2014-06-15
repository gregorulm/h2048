h2048
=====
A Haskell implementation of the game 2048

Gregor Ulm
2014-06-15


The original version of 2048 can be played online here:
gabrielecirulli.github.io/2048/


This code below covers the game logic, and allows to play 2048
on the command line.


Rules:

- the starting position is always the same
- there are 4 moves: up, down, left, right; those
    inputs move all tiles in the respective direction
- two adjacent tiles with the same number merge into one
    tile that holds the sum of those numbers
- if, after a move, the board position has changed, one of
    the free tiles will turn into a 2 or a 4
- the game is over when the player has no move left, or
    when the number 2048 has been reached


Controls:

- the game uses the WASD control scheme, i.e. to move all tiles
    up, you would have to press W (followed by ENTER) etc.


Execution:

- start the program by calling 'main'


Notes:

- this is a prototype with a minimal terminal window UI
- it has only been tested in GHCi 7.6.3 in Apple OS X
