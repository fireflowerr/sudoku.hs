# sudokuhs
A sudoku game written in Haskell!

# Screencast
![demo](https://i.imgur.com/YhDexXX.gif)

## About
This is a simple sudoku game built with gtk3 using the haskell-gi
library. Sudokhs does not require any internet connection and is capable of
generating, validating, and solving sudoku tables.

## Building
`cabal build`

## Installing
`cabal install`

Aditional binary (x86_64) packages are provided for Arch and Debian distributions
You can download the appropriate package from the [release](https://github.com/paroxayte/sudoku.hs/releases/tag/0.1.0) page.

**Arch:** `pacman -U sudokuhs-1-1-x86_64.pkg.tar.xz`
<BR/>
**Debian:** `dpkg -i sudokuhs_0.1-0.deb`
<BR/>
**MacOS:** *requires [brew](https://brew.sh/):* `brew install gtk+3 fontconfig` -> Double click on the .dmg (downloaded from the release page).

## Controls
* (✓) - Highlights all conflicts relative to selected cell
* (x) - Clear the selected cell
* (?) - Solve the selected cell if possible\*
* new game - Starts a new game at the selected difficulty level
* check    - If the sudoku table contains errors, highlight them
* solve    - Write a solution to the sudoku table if possible 

*note: ? - Possible indicates the grid contains no conflicts other than
  potentially the selected cell*
<BR/>
*note: solve - Possible indicates the grid contains no conflicts*
