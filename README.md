# sudokuhs
A sudoku game written in Haskell!

# Screencast
![demo](https://i.imgur.com/YhDexXX.gif)

## About
This is a simple sudoku game built with gtk3 using the haskell-gi
library. Sudokhs does not require any internet connection and is capabe of
generating, validating, and solving sudoku tables.

## Controls
* (✓) - Highlights all conflicts relative to selected cell
* (x) - Clear the selected cell
* (?) - Solve the selected cell if possible\*
* new game - Starts a new game at the selected difficulty level
* check    - If the sudoku table contains errors, highlight them
* solve    - Write a solution to the sudoku table if possible \*
*note: ? - Possible indicates the grid contains no conflicts other than
  potentially the selected cell*
*note: solve - Possible indicates the grid contains no conflicts*
