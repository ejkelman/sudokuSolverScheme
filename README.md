# sudokuSolverScheme

# Sudoku Solver (Racket / Scheme)

A complete Sudoku solver implemented in Racket using functional programming
paradigms

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


## Notes

Built as a functional-programming exercise: the goal was to solve the problem
using pure functions and recursion — no in-place array mutation — while
keeping the constraint logic and search logic cleanly separated.
