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


