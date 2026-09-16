# sudokuSolverScheme

# Sudoku Solver (Racket / Scheme)

A complete Sudoku solver implemented in Racket using functional programming
paradigms.

## How it works

The solver combines two strategies:
- **Constraint propagation**: for each empty cell, candidate values are narrowed
  based on row, column, and 3x3 box constraints before any guessing happens.
- **Recursive backtracking search**: when constraint propagation alone can't
  fill the board, the solver recursively tries candidate values for the most
  constrained cell and backtracks on failure.

## Key components

- `strings->puzzle` — parses a raw string puzzle format into an internal
  board representation, handling both fixed (given) values and open cells
  with candidate sets.
- `all-satisfy?` / `any-satisfy?` — predicate-based utilities for checking
  constraints across rows, columns, and boxes.
- `find-where` — locates cells matching a predicate across the 2D board,
  used to find the next cell to branch on during search.
- Core solve loop — ties constraint propagation and backtracking together
  into a single recursive solve function.

## Usage

```racket
(require "sudoku-solver.rkt")

(solve (strings->puzzle puzzle-string))
```
where puzzle-string is a list of 9 strings of length 9, composed of digits and questions marks (for blank entries).

## Example

(check-expect
 (solve-sudoku
  (list "53??7????"
        "6??195???"
        "?98????6?"
        "8???6???3"
        "4??8?3??1"
        "7???2???6"
        "?6????28?"
        "???419??5"
        "????8??79"))
 
 (list
  (list 5 3 4 6 7 8 9 1 2)
  (list 6 7 2 1 9 5 3 4 8)
  (list 1 9 8 3 4 2 5 6 7)
  (list 8 5 9 7 6 1 4 2 3)
  (list 4 2 6 8 5 3 7 9 1)
  (list 7 1 3 9 2 4 8 5 6)
  (list 9 6 1 5 3 7 2 8 4)
  (list 2 8 7 4 1 9 6 3 5)
  (list 3 4 5 2 8 6 1 7 9)))
 

## Notes

Built as a functional-programming exercise: the goal was to solve the problem
using pure functions and recursion, without array mutation or loops.
