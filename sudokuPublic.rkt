;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudokuPublic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; checks if all entries in a matrix satisfy
;; a given predicate
(define (all-satisfy? pred? matrix)
  (empty?
   (filter
    (lambda (x)
      (cond
        [(not (list? x))
         (not (pred? x))]
        [else (not (all-satisfy? pred? x))]))
    matrix)))

(check-expect (all-satisfy? integer? '((2 3 4) (5 6 7)))
true)
(check-expect (all-satisfy? integer? '((2 3 4) (5 (six) 7)))
false)


(define (any-satisfy? pred? matrix)
  (not (all-satisfy?
        (lambda (x) (not (pred? x)))
        matrix)))

(check-expect (any-satisfy? symbol? '((2 3 4) (5 6 7)))
false)
(check-expect (any-satisfy? symbol? '((2 3 4) (5 six 7)))
true)

;; finds the index of the first entry
;; in a matrix that satisfies a given predicate
(define (find-where pred matrix)
  (local
    [(define (what-row? pred rows row-tracker)
       (cond
         [(empty? rows) empty]
         [else
          (local
            [(define (which-column?
                      pred row column-tracker)
              (cond
               [(empty? row) empty]
               [(pred (first row)) column-tracker]
               [else (which-column?
                      pred (rest row) (add1 column-tracker))]))]
            (local
              [(define final-result
                 (which-column? pred (first rows) 0))]
               (cond
                 [(number? final-result)
                  (list final-result row-tracker)]
                 [else (what-row? pred (rest rows) (add1 row-tracker))])))]))]
    (what-row? pred matrix 0)))
    

(define wherematrix '(( 1 2 3 4 )
                      ( 4 5 (3 6) (1 2) )
                      ( (7) 8 9 () )))

(check-expect (find-where list? wherematrix) '(2 1))
(check-expect (find-where empty? wherematrix) '(3 2))
(check-expect (find-where integer? wherematrix) '(0 0))



;; A Cell is a (anyof (listof Nat) Nat)
;; Requires: Nat values are positive.
;; List values contain no duplicates, and are in increasing order.
;; A Puzzle is a (matrixof Cell)
;; A Single is a (list Nat). That is, a list of length exactly 1.
;; A Solution is a (matrixof Nat).

(define (char->number c)
            (cond
              [(char=? c #\0) 0]
              [(char=? c #\1) 1]
              [(char=? c #\2) 2]
              [(char=? c #\3) 3]
              [(char=? c #\4) 4]
              [(char=? c #\5) 5]
              [(char=? c #\6) 6]
              [(char=? c #\7) 7]
              [(char=? c #\8) 8]
              [(char=? c #\9) 9]))

(define (strings->puzzle strings)
    (local
      [(define (each-string string)
         (cond
       [(empty? (string->list string)) empty]
       [(and (>= (char->integer
                  (first (string->list string))) 48)
             (<= (char->integer
                  (first (string->list string))) 57))
        (cons (list (char->number
                     (first (string->list string))))
              (each-string (list->string
                                (rest (string->list string)))))]
        [else
          (cons (build-list
                 (length (string->list (first strings)))
                 (lambda (x) (add1 x)))
                (each-string (list->string
                              (rest (string->list string)))))]))]
   (map
    (lambda (string2) (each-string string2))
    strings)))

(check-expect (strings->puzzle '("????56789"
                  "12???????"
                  "?????????"
                  "213489756"
                  "?????????"
                  "?????????"
                  "????0????"
                  "?????????"
                  "?????????"))
              (list
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 5)
                (list 6)
                (list 7)
                (list 8)
                (list 9))
               (list
                (list 1)
                (list 2)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list (list 2) (list 1) (list 3) (list 4) (list 8) (list 9) (list 7) (list 5) (list 6))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 0)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))
               (list
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9)
                (list 1 2 3 4 5 6 7 8 9))))


(check-expect (strings->puzzle '("???"
                                 "?3?"
                                 "??2"))
              '(( (1 2 3) (1 2 3) (1 2 3) )
                ( (1 2 3) (3) (1 2 3) )
                ( (1 2 3) (1 2 3) (2) )))

(check-expect (strings->puzzle '("??3?"
                                 "??2?"
                                 "?4??"
                                 "????"))
              '(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
                ( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
                ( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
                ( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) )))



(define (any-single? puzzle)
  (not (empty?
        (find-where
         (lambda (x)
           (and (list? x)
           (= (length x) 1))) puzzle))))

(check-expect (any-single? '(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) ))) true)



(define (remove-singles puzzle)
  (cond
    [(not (any-single? puzzle)) puzzle]
    [else
     (local
       [(define first-single
          (find-where (lambda (cell)
                        (and (list? cell)
                             (= (length cell) 1))) puzzle))
        (define column-single (first first-single))
        (define row-single (second first-single))
        (define number (local
                         [(define (get-row puz row-num)
                            (cond
                              [(= row-num 0) (first puz)]
                              [else (get-row (rest puz) (sub1 row-num))]))
                          (define (get-col r col-num)
                            (cond
                              [(= col-num 0) (first r)]
                              [else (get-col (rest r) (sub1 col-num))]))]
                         (first (get-col (get-row puzzle row-single) column-single))))
        (define updated-puzzle (map
                            (lambda (row row-number)
                              (map
                               (lambda (cell col-number)
                                 (cond
                                   [(and (= row-number row-single)
                                         (= col-number column-single))
                                    number]
                                   [(and (list? cell)
                                         (or (= row-number row-single)
                                             (= col-number column-single)))
                                    (filter (lambda (x) (not (= x number))) cell)]
                                   [else cell]))
                               row (build-list (length row)
                                               (lambda (x) x))))
                              puzzle (build-list (length puzzle)
                                                 (lambda (x) x))))]
       (remove-singles updated-puzzle))]))

(check-expect (remove-singles (strings->puzzle '("??3?"
                                                 "??2?"
                                                 "?4??"
                                                 "????")))
              '(( (1 2 4) (1 2) 3 (1 2 4) )
                ( (1 3 4) (1 3) 2 (1 3 4) )
                ( (2 3) 4 1 (2 3) )
                ( (1 2 3) (1 2 3) 4 (1 2 3) )))

(define (diagonal-has-2? p)
  (and (not (empty? p))
       (or (= 2 (first (first p)))
           (diagonal-has-2? (map rest (rest p))))))

(check-expect (diagonal-has-2? '((3 2 1)
                                 (2 1 3)
                                 (1 3 1))) false)



;; single row, no clues at all
(check-expect (strings->puzzle '("??"))
              '(( (1 2) (1 2) )))
 
;; fully-clued puzzle produces all singles
(check-expect (strings->puzzle '("12" "21"))
              '(( (1) (2) )
                ( (2) (1) )))
 
(check-expect (any-single? '(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) ))) true)
 
;; no singles anywhere -> false
(check-expect (any-single? '(( (1 2) (1 2) )
                             ( (1 2) (1 2) ))) false)
 
;; puzzle of already-solved numbers (no lists) -> false
(check-expect (any-single? '((1 2) (2 1))) false)
 
 
(check-expect (remove-singles (strings->puzzle '("??3?"
                                                 "??2?"
                                                 "?4??"
                                                 "????")))
              '(( (1 2 4) (1 2) 3 (1 2 4) )
                ( (1 3 4) (1 3) 2 (1 3 4) )
                ( (2 3) 4 1 (2 3) )
                ( (1 2 3) (1 2 3) 4 (1 2 3) )))
 
;; remove-singles on an already-fully-solved puzzle
;; is a no-op (nothing left to propagate)
(check-expect (remove-singles '((1 2) (2 1)))
              '((1 2) (2 1)))
 
(check-expect (diagonal-has-2? '((3 2 1)
                                 (2 1 3)
                                 (1 3 1))) false)
 
;; diagonal does have a 2
(check-expect (diagonal-has-2? '((2 1 3)
                                 (1 3 2)
                                 (3 2 1))) true)
 
 

;; cell-at : Puzzle Nat Nat -> Cell
;; gets the cell at (row, col)
(define (cell-at puzzle row col)
  (local
    [(define (get-row puz row-num)
       (cond
         [(= row-num 0) (first puz)]
         [else (get-row (rest puz) (sub1 row-num))]))
     (define (get-col r col-num)
       (cond
         [(= col-num 0) (first r)]
         [else (get-col (rest r) (sub1 col-num))]))]
    (get-col (get-row puzzle row) col)))
 
(check-expect (cell-at wherematrix 1 2) (list 3 6))
(check-expect (cell-at wherematrix 0 0) 1)
 
;; last row, last column
(check-expect (cell-at wherematrix 2 3) empty)
 
 
;; set-cell : Puzzle Nat Nat Cell -> Puzzle
;; replaces the cell at (row, col) with value, leaving everything else alone
(define (set-cell puzzle row col value)
  (map
   (lambda (r row-number)
     (map
      (lambda (cell col-number)
        (cond
          [(and (= row-number row) (= col-number col)) value]
          [else cell]))
      r (build-list (length r) (lambda (x) x))))
   puzzle (build-list (length puzzle) (lambda (x) x))))
 
(check-expect (set-cell (list (list 1 2) (list 3 4)) 0 1 9)
              (list (list 1 9) (list 3 4)))
 
;; setting a cell doesn't disturb other rows
(check-expect (set-cell (list (list 1 2) (list 3 4)) 1 0 99)
              (list (list 1 2) (list 99 4)))
 
 
;; has-contradiction? : Puzzle -> Boolean
;; true if some cell has been reduced to zero candidates
(define (has-contradiction? puzzle)
  (not (empty?
        (find-where (lambda (cell) (and (list? cell) (empty? cell))) puzzle))))
 
(check-expect (has-contradiction? (list (list 1 2) (list 3 4))) false)
(check-expect (has-contradiction? (list (list 1 empty) (list 3 4))) true)
 
;; a fully-solved puzzle has no contradiction
(check-expect (has-contradiction? '((1 2) (2 1))) false)
 
 
;; all-solved? : Puzzle -> Boolean
;; true when every cell has been narrowed down to a single number
(define (all-solved? puzzle)
  (empty? (find-where list? puzzle)))
 
(check-expect (all-solved? (list (list 1 2) (list 3 4))) true)
(check-expect (all-solved? (list (list 1 (list 2 3)) (list 3 4))) false)
 
;; single remaining candidate lists still count as "not solved"
(check-expect (all-solved? (list (list 1 (list 2)) (list 3 4))) false)
 
 
;; contains? / has-duplicate? / only-numbers : helpers for validity checking
(define (contains? x lst)
  (cond
    [(empty? lst) false]
    [(equal? x (first lst)) true]
    [else (contains? x (rest lst))]))
 
(check-expect (contains? 3 (list 1 2 3)) true)
(check-expect (contains? 5 (list 1 2 3)) false)
(check-expect (contains? 1 empty) false)
 
(define (has-duplicate? lst)
  (cond
    [(empty? lst) false]
    [(contains? (first lst) (rest lst)) true]
    [else (has-duplicate? (rest lst))]))
 
(define (only-numbers cells)
  (filter (lambda (x) (not (list? x))) cells))
 
(check-expect (has-duplicate? (only-numbers (list 1 2 (list 3 4) 1))) true)
(check-expect (has-duplicate? (only-numbers (list 1 2 (list 3 4) 3))) false)
 
;; only-numbers filters out all list-valued cells
(check-expect (only-numbers (list 1 (list 2 3) 4 (list 5))) (list 1 4))
 
;; has-duplicate? on an empty list is false
(check-expect (has-duplicate? empty) false)
 
 
;; transpose : Puzzle -> Puzzle
;; the puzzle read column-by-column instead of row-by-row
(define (transpose puzzle)
  (cond
    [(empty? (first puzzle)) empty]
    [else (cons (map first puzzle) (transpose (map rest puzzle)))]))
 
(check-expect (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))
 
;; transposing a non-square matrix
(check-expect (transpose (list (list 1 2) (list 3 4) (list 5 6)))
              (list (list 1 3 5) (list 2 4 6)))
 
;; transposing twice gives back the original
(check-expect (transpose (transpose (list (list 1 2) (list 3 4))))
              (list (list 1 2) (list 3 4)))
 
 
;; box-size : Puzzle -> Nat
;; side length of one box. If the puzzle's dimension isn't a perfect
;; square (e.g. a 2x2 test grid), boxes aren't well-defined, so we
;; fall back to size 1 (each cell its own box -> box check never fires).
(define (box-size puzzle)
  (local
    [(define n (length puzzle))
     (define s (inexact->exact (floor (sqrt n))))]
    (cond
      [(= (* s s) n) s]
      [else 1])))
 
(check-expect (box-size (list (list 1 2 3 4) (list 5 6 7 8)
                               (list 9 10 11 12) (list 13 14 15 16))) 2)
(check-expect (box-size (list (list 1 2) (list 3 4))) 1)
 
;; 9x9 grid gives box size 3
(check-expect (box-size (build-list 9 (lambda (x) (build-list 9 (lambda (y) 1))))) 3)
 
;; 3x3 grid (not a perfect square dimension) falls back to 1
(check-expect (box-size (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) 1)
 
 
;; box-of : Puzzle Nat Nat -> (listof Cell)
;; all cells in the box containing (row, col)
(define (box-of puzzle row col)
  (local
    [(define size (box-size puzzle))
     (define box-row-start (* size (quotient row size)))
     (define box-col-start (* size (quotient col size)))
     (define (cells-in-box r-offset c-offset)
       (cond
         [(= r-offset size) empty]
         [(= c-offset size) (cells-in-box (add1 r-offset) 0)]
         [else
          (cons (cell-at puzzle (+ box-row-start r-offset) (+ box-col-start c-offset))
                (cells-in-box r-offset (add1 c-offset)))]))]
    (cells-in-box 0 0)))
 
(check-expect (box-of (list (list 1 2 3 4) (list 5 6 7 8)
                             (list 9 10 11 12) (list 13 14 15 16)) 0 0)
              (list 1 2 5 6))
(check-expect (box-of (list (list 1 2 3 4) (list 5 6 7 8)
                             (list 9 10 11 12) (list 13 14 15 16)) 3 3)
              (list 11 12 15 16))
 
;; box-of on a 2x2 grid (fallback box size 1) is just the cell itself
(check-expect (box-of (list (list 1 2) (list 3 4)) 0 1) (list 2))
(check-expect (box-of (list (list 1 2) (list 3 4)) 1 0) (list 3))
 
;; a different box within a 4x4 grid
(check-expect (box-of (list (list 1 2 3 4) (list 5 6 7 8)
                             (list 9 10 11 12) (list 13 14 15 16)) 1 2)
              (list 3 4 7 8))
 
 
;; valid-puzzle? : Puzzle -> Boolean
;; true when no row, column, or box has two identical solved numbers
(define (all-groups-valid? groups)
  (cond
    [(empty? groups) true]
    [(has-duplicate? (only-numbers (first groups))) false]
    [else (all-groups-valid? (rest groups))]))
 
(check-expect (all-groups-valid? (list (list 1 2 3) (list 4 5 6))) true)
(check-expect (all-groups-valid? (list (list 1 2 1) (list 4 5 6))) false)
 
(define (all-boxes-valid? puzzle)
  (local
    [(define size (box-size puzzle))
     (define (check-boxes box-row box-col)
       (cond
         [(= box-row size) true]
         [(= box-col size) (check-boxes (add1 box-row) 0)]
         [(has-duplicate?
           (only-numbers (box-of puzzle (* box-row size) (* box-col size))))
          false]
         [else (check-boxes box-row (add1 box-col))]))]
    (check-boxes 0 0)))
 
(check-expect (all-boxes-valid? (list (list 1 2 3 4) (list 5 6 7 8)
                                       (list 9 10 11 12) (list 13 14 15 16))) true)
(check-expect (all-boxes-valid? (list (list 1 2 3 4) (list 5 1 7 8)
                                       (list 9 10 11 12) (list 13 14 15 16))) false)
 
(define (valid-puzzle? puzzle)
  (and (all-groups-valid? puzzle)
       (all-groups-valid? (transpose puzzle))
       (all-boxes-valid? puzzle)))
 
(check-expect (valid-puzzle? (list (list 1 2) (list 3 4))) true)
(check-expect (valid-puzzle? (list (list 1 1) (list 3 4))) false)
 

(check-expect (valid-puzzle? (list (list 1 2) (list 1 4))) false)
 
;; a valid 4x4 grid with candidate-lists mixed in
;; (only-numbers, should ignore the unsolved cells)
(check-expect (valid-puzzle? (list (list 1 2 3 4)
                                    (list 3 4 1 2)
                                    (list (list 2 4) 1 4 3)
                                    (list 4 3 2 1))) true)
 
;; a 9x9 grid with a box conflict
;; (two 5s in the same 3x3 box) but no row/column conflict
(check-expect
 (valid-puzzle?
  (list (list 1 2 3 4 5 6 7 8 9)
        (list 4 5 6 7 8 9 1 2 3)
        (list 7 8 9 1 2 3 4 5 6)
        (list 2 3 1 5 6 4 8 9 7)
        (list 5 6 4 8 9 7 2 3 1)
        (list 8 9 7 2 3 1 5 6 4)
        (list 3 1 2 6 4 5 9 7 8)
        (list 6 4 5 9 7 8 3 1 2)
        (list 9 7 8 3 1 2 6 4 5)))
 true)
 
 
;; best-guess-location : Puzzle -> (anyof (list Nat Nat Nat) empty)
;; finds the un-solved cell with the fewest remaining candidates,
;; as (list col row num-candidates); empty if the puzzle is fully solved
(define (best-guess-location puzzle)
  (local
    [(define (scan-row row col-tracker best)
       (cond
         [(empty? row) best]
         [(and (list? (first row))
               (> (length (first row)) 1)
               (or (empty? best) (< (length (first row)) (third best))))
          (scan-row (rest row) (add1 col-tracker)
                    (list col-tracker 0 (length (first row))))]
         [else (scan-row (rest row) (add1 col-tracker) best)]))
     (define (fix-row-number entry row-tracker)
       (cond
         [(empty? entry) entry]
         [else (list (first entry) row-tracker (third entry))]))
     (define (scan-rows rows row-tracker best)
       (cond
         [(empty? rows) best]
         [else
          (local
            [(define row-best (scan-row (first rows) 0 empty))
             (define updated-best
               (cond
                 [(empty? row-best) best]
                 [(empty? best) (fix-row-number row-best row-tracker)]
                 [(< (third row-best) (third best))
                  (fix-row-number row-best row-tracker)]
                 [else best]))]
            (scan-rows (rest rows) (add1 row-tracker) updated-best))]))]
    (scan-rows puzzle 0 empty)))
 
(check-expect (best-guess-location (list (list 1 (list 2 3)) (list 3 4)))
              (list 1 0 2))
 

(check-expect (best-guess-location (list (list 1 2) (list 3 4))) empty)
 
(check-expect (best-guess-location (list (list 1 (list 2 3 4)) (list (list 5 6) 4)))
              (list 0 1 2))
 
 
;; solve-puzzle : Puzzle -> (anyof Solution false)
;; propagates constraints, then guesses and backtracks on the
;; most-constrained cell until the puzzle is solved or proven impossible
(define (solve-puzzle puzzle)
  (local
    [(define propagated (remove-singles puzzle))]
    (cond
      [(has-contradiction? propagated) false]
      [(not (valid-puzzle? propagated)) false]
      [(all-solved? propagated) propagated]
      [else
       (local
         [(define guess (best-guess-location propagated))
          (define guess-col (first guess))
          (define guess-row (second guess))
          (define candidates (cell-at propagated guess-row guess-col))]
         (try-candidates candidates propagated guess-row guess-col))])))
 
;; try-candidates : (listof Nat) Puzzle Nat Nat -> (anyof Solution false)
;; tries each candidate value for (row, col) in turn
(define (try-candidates candidates puzzle row col)
  (cond
    [(empty? candidates) false]
    [else
     (local
       [(define attempt (solve-puzzle (set-cell puzzle row col (first candidates))))]
       (cond
         [(false? attempt) (try-candidates (rest candidates) puzzle row col)]
         [else attempt]))]))
 
 
;; solve-sudoku : (listof String) -> (anyof Solution false)
;; convenience wrapper matching strings->puzzle's input format
(define (solve-sudoku strings)
  (solve-puzzle (strings->puzzle strings)))
 
(check-expect (solve-sudoku (list "1234" "3412" "2143" "432?"))
              (list (list 1 2 3 4) (list 3 4 1 2) (list 2 1 4 3) (list 4 3 2 1)))
 
(check-expect (solve-sudoku (list "12??" "34??" "?1?3" "43?1"))
              (list (list 1 2 3 4) (list 3 4 1 2) (list 2 1 4 3) (list 4 3 2 1)))
 
(check-expect (solve-sudoku (list "1?" "?1"))
              (list (list 1 2) (list 2 1)))
 
;; an unsolvable 4x4 puzzle (two identical rows given
;; as fixed clues can't both be valid Latin-square rows) returns false
(check-expect (solve-sudoku (list "1234" "1234" "????" "????"))
              false)
 
;; a harder 9x9 puzzle with only a few clues,
;; requiring real backtracking (not just propagation) to solve.
;; This exercises box-checking on a real 3x3-box-sized grid.
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
 
