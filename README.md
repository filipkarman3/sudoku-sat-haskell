A sudoku solver but ridiculously more complicated than it has to be

Converts sudoku hints to a SAT instance, then solves that

Input sudoku hints on separate lines into sudoku.txt in the following format:

x y n

where x is the x-co-ordinate, 0 is the left-most

y is the y-co-ordinate, 0 is the top-most

n is the number in the square


For example:

0 0 1

0 1 6

5 2 3

Run with `cabal run`

Tested with ghc 9.4.8