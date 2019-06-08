;2.1

(define (make-rat n d)
  (let (
        (g (gcd n d))
        (abs-n (abs n))
        (abs-d (abs d))
       )
    (if (> (* n d) 0)
      (cons abs-n abs-d)
      (cons (* -1 abs-n) abs-d)
    )
  )
)

;2.2

(define (make-segment s-point e-point)
  (cons s-point e-point))

(define (start-segment line-seg)
  (car line-seg))

(define (end-segment line-seg)
  (cdr line-seg))


(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))


(define (midpoint-segment line-seg)
  (let (
        (s-point (start-segment line-seg))
        (e-point (end-segment line-seg))
        (avg (lambda (x y) (/ (+ x y) 2)))
        )
    (cons (avg (x-point s-point) (x-point e-point))
          (avg (y-point s-point) (y-point e-point))
    )
  )
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


;2.3

(define (make-rect p1 p2 p3 p4)
  (cons (cons p1 p2) (cons p3 p4)))

(define (get-point rect n)
  (cond ((= n 1) (car (car rect)))
        ((= n 2) (cdr (car rect)))
        ((= n 3) (car (cdr rect)))
        (else (cdr (cdr rect)))))

(define (get-perimeter rect)
  (let (
        (dist (lambda (p1 p2)
                (sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2)))))))
        )
    (* 2 (+ (dist (get-point rect 1) (get-point rect 2)) (dist (get-point rect 2) (get-point rect 3))))
  )
)

(define (get-area rect)
  (let (
        (dist (lambda (p1 p2)
                (sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2)))))))
        )
    (* (dist (get-point rect 1) (get-point rect 2)) (dist (get-point rect 2) (get-point rect 3)))
  )
)

;2.4

;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

;2.5
(define (exp n p)
  (define (iter k result)
    (if (not (< 0 k))
      result
      (iter (- k 1) (* result n))))
  (iter p 1)
)

;(define (cons a b) (* (exp 2 a) (exp 3 b))) 

(define (num-of-mul z divisor)
  (define (iter n k)
    (if (= (remainder n divisor) 0)
      (iter (/ n divisor) (+ k 1))
      k)
  )
  (iter z 0)
)

;(define (car z)
;  (num-of-mul z 2))

;(define (cdr z)
;  (num-of-mul z 3))

;2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;2.7
;(define (cons x y) (lambda (m) (m x y)))
;(define (car z) (z (lambda (p q) p)))
;(define (cdr z) (z (lambda (p q) q)))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))
(define (mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))


;2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y)))
       )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;2.10
(define (div-interval x y)
  (define include-zero? (lambda (x) (or (= (lower-bound x) 0) (= (upper-bound x) 0))))
  (if (include-zero? y)
    (display "second argument has zeros, so it can't be calculated")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))


;2.12
(define (add-interval x y)
  (cons (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c t)
  (let ((w (* c (/ t 100.0))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))


;2.14, 2.15
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define i1 (make-center-percent 3 20))
(define i2 (make-center-percent 5 10))

(percent (div-interval i1 i1))
; > 38.461 almost 20(i1 percent) + 20(i1 percent)

(percent (div-interval i1 i2))
; > 29.411 almost 20(i1 percent) + 10(i2 percent)

; the result of div-interval gets its tolerance as sum of intervals'
; so the function which uses less div-intervals or div-interval with zero tolerance(e.g. one)
; makes more tight interval.

;2.17
(define test-list (list 1 2 3 4 5))
(define (last-pair arr)
  (if (null? (cdr arr))
    (car arr)
    (last-pair (cdr arr))
  )
)

;2.18
(define (reverse arr)
  (define (iter remain result)
    (if (null? (cdr remain))
      (cons (car remain) result)
      (iter (cdr remain) (cons (car remain) result))))
  (iter arr (list )))

;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

;2.20
(define (same-parity . l)
  (define first-e (car l))
  (define (is-same-parity? n) (= (remainder first-e 2) (remainder n 2)))
  (define (iter remain)
    (cond ((null?  remain) remain)
          ((is-same-parity? (car remain)) (cons (car remain) (iter (cdr remain))))
          (else (iter (cdr remain))))
  )
  (iter l)
)

;2.21
(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
    items
    (cons (* (car items) (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (e) (square e)) items))

;2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
            ; (cons answer
            ;       (square (car things))))
            ; this isn't what list structure defined.
            ; in this case, cdr doen't point to next pointer
            ; we need proc which append element to end of list,
            ; but in this strucutre this operation needs O(n).
    (iter items (list )))

;2.23
(define (for-each proc items)
  (define (iter arr buffer)
    (if (null? arr)
      true
      (iter (cdr arr) (proc (car arr)))))
  (iter items true))

(define (for-each proc items)
  (define proc-iter (lambda (a b) b a))
  (define (iter arr)
    (if (null? arr)
      true
      ; second argument is evaluated first.
      (proc-iter (iter (cdr arr)) (proc (car arr)))))
  (iter items))


;2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) x)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;(1 (2 (3 4)))
;1 (2 (3 4)
;   2 (3 4)
;      3 4

;2.25
(define prob1 (list 1 3 (list 5 7) 9))
(define sol1 (lambda (items)
               (car (cdr (car (cdr (cdr items)))))))

(define prob2 (list (list 7)))
(define sol2 (lambda (items)
               (car (car items))))

(define prob3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(define sol3 (lambda (items)
               (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items))))))))))))))

;2.26
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;( 1 2 3 4 5 6)
(cons x y); ((1 2 3) 4 5 6)
(list x y); ((1 2 3) (4 5 6)) 


;2.27
(define (deep-reverse tree)
  (define (iter items result)
    (if (null? items)
      result
      (iter (cdr items)
            (cons (if (pair? (car items))
                    (deep-reverse (car items))
                    (car items))
                  result))))
  (iter tree (list))
)


;2.28
(define x (list (list 1 2) (list 3 4)))
(define (fringe items)
  (cond ((null? items) items)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items)))))
)

;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define sub-br-l (make-branch 8 2))
(define sub-br-r (make-branch 4 8))
(define mobile-sub (make-mobile sub-br-l sub-br-r))

(define br-l (make-branch 8 10))
(define br-r (make-branch 4 mobile-sub))
(define mobile-a (make-mobile br-l br-r))

;b
(define (total-weight mobile)
  (let ((left-st (branch-structure (left-branch mobile)))
        (right-st (branch-structure (right-branch mobile))))
    (+ (if (pair? left-st) (total-weight left-st) left-st)
       (if (pair? right-st) (total-weight right-st) right-st))
  )
)

;c
(define (balanced mobile)
  (let ((left-st (branch-structure (left-branch mobile)))
        (right-st (branch-structure (right-branch mobile))))
    (= (if (pair? left-st) (total-weight left-st) left-st)
       (if (pair? right-st) (total-weight right-st) right-st))
  )
)

;d
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define sub-br-l (make-branch 8 2))
(define sub-br-r (make-branch 4 8))
(define mobile-sub (make-mobile sub-br-l sub-br-r))

(define br-l (make-branch 8 10))
(define br-r (make-branch 4 mobile-sub))
(define mobile-a (make-mobile br-l br-r))


;2.30
(define test (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))
  
;2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))


;2.32
(define (subsets s)
  (display s)
  (newline)
  (if (null? s)
    (list s)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (e) (append e (list (car s)))) rest)))
  )
)

;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    (list)
    (cons low (enumerate-interval (+ low 1) high))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms))
              )
              0
              coefficient-sequence))


;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (e) (if (pair? e) (count-leaves e) 1)) t)))


;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    (list)
    (cons (accumulate op init (map (lambda (e) (car e)) seqs))
          (accumulate-n op init (map (lambda (e) (cdr e)) seqs)))))

(define acc-n-test (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;2.37
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(/ 1 (/ 2 (/ 3 1)))

(fold-left / 1 (list 1 2 3))
(/ (/ (/ 1 1) 2) 3)

(fold-right list (list) (list 1 2 3))
(list 1 (list 2 (list 3 (list))))

(fold-left list (list) (list 1 2 3))
(list (list (list (list) 1) 2) 3)
; condition: (= (op x y) (op y x))

;2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))


;2.40
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (define (pair-sum pair) (accumulate + 0 pair))
  (define (make-pair-sum pair) (list (car pair) (cadr pair) (pair-sum pair)))
  (define (prime? n) (define (iter k) (cond ((= k n) true)
                                            ((= (remainder n k) 0) false)
                                            (else (iter (+ k 1)))))
    (iter 2))
  (define (prime-sum? pair) (prime? (pair-sum pair)))
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;2.41
(define (find-triples n s)
  (define (triple-sum pair) (accumulate + 0 pair))
  (filter (lambda (triple) (= s (triple-sum triple)))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;2.42
;index of list is col number of each position
(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (if (>= 1 k) 
    (cons new-row (if (null? rest-of-queens) rest-of-queens (cdr rest-of-queens)))
    (cons (car rest-of-queens) (adjoin-position new-row (- k 1) (cdr rest-of-queens)))))

(define (safe? k pos)
  (define (get-e n items)
    (cond ((>= 1 n) (car items))
          ((null? items) (list))
          (else (get-e (- n 1) (cdr items)))))
  
  (define (iter k k-element rest-pos)
    (let ((q-pos (car rest-pos)))
      (if (>= 1 k)
        true
        (and (not (or (= k-element (+ q-pos (- k 1)))
                      (= k-element (- q-pos (- k 1)))
                      (= k-element q-pos)))
             (iter (- k 1) k-element (cdr rest-pos))))))

  (let ((k-element (get-e k pos)))
    (iter k k-element pos))
)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;2.43

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;2.44 ~ 2.52 skip (non-supported procedures were used)

;2.53

; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f
; #f
; (red shoes blue socks)

;2.54
(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        (else (and (if (and (pair? (car a)) (pair? (car b)))
                     (equal? (car a) (car b))
                     (eq? (car a) (car b)))
                   (equal? (cdr a) (cdr b))))))

;2.55
(car ''abracadabra) ; (quote abracadabra)


