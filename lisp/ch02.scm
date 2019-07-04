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
        (else (cond ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
                                                    (equal? (cdr a) (cdr b))))
                    ((or (pair? a) (pair? b)) false)
                    (else (eq? a b))))))

;2.55
(car ''abracadabra) ; (quote abracadabra)

;2.56 ~ 2.58 skip

;2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (union-set x y)
  (if (null? y)
    x
    (union-set (if (element-of-set? (car y) x) x (cons (car y) x))
               (cdr y))))

;2.60
; it doesn't need to check element-of-set? when operated.
; adjoin and union can be boosted.

;2.61
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            (else (intersection-set set1 (cdr set2)))))))

;2.61
(define (adjoin-set x set)
  (if (null? set)
      (cons x '())
      (let ((y (car set)))
        (cond ((= x y) set)
            ((< x y) (cons x set))
            (else (cons y (adjoin-set x (cdr set))))))))

;2.62
(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
    (if (null? set2) set1 set2)
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
            ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
            (else (cons x1 (union-set (cdr set1) set2)))))))

;2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry tree)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

;a
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (display result)
    (newline)
    (if (null? tree)
      result
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '())
)

;same, one is recursive the other one is iterative.

;b
;tree->list-1: O(n * log(n))
;tree->list-2: O(n)
;difference is caused by append operation

;skip 64 and 65

;2.66
(define (lookup given-key database)
  (cond ((null? database) false)
        ((equal? given-key (key (entry database))) (entry database))
        ((> given-key (key (entry database))) (lookup given-key (left-branch database)))
        ((< given-key (key (entry database))) (lookup given-key (right-branch database)))))

;.267
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCHH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (a d a b b c a)

;2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (is-in? item items)
    (if (null? items)
      false
      (or (eq? (car items) item)
          (is-in? item (cdr items)))))

  (define (is-left? branch)
    (cond ((is-in? symbol (symbols (left-branch branch))) true)
          ((is-in? symbol (symbols (right-branch branch))) false)
          (else (error "no element in tree" symbol))))

  (if (leaf? tree)
    '()
    (let ((is-left-branch? (is-left? tree)))
      (cons (if is-left-branch? '0 '1)
            (encode-symbol symbol (if is-left-branch? (left-branch tree)
                                                      (right-branch tree)))))))

;2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (make-code-tree (car rest) result) (cdr rest))))

  (if (null? leaves)
      '()
      (iter (car leaves) (cdr leaves))))

(define test-pairs (list (list 'a 2) (list 'b 1) (list 'c 4) (list 'd 5)))


; skip 2.70 ~ 2.72
(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                     (multiplier exp)
                     (deriv (multiplicand exp) var))
                   (make-product
                     (deriv (multiplier exp) var)
                     (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))


; 2.73
; data-directed style
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a
; variable and number doesn't have operator, so it violates the rules of table.
; it is possible to make a table for each predicate, but it seems unnecassary.

; b
(define (install-sum-and-product-package)
  (define (=number? exp num) (and (number? exp) (= exp num)))

  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (mul-deriv exp var)
    (sum-deriv (make-product
                 (multiplier exp)
                 (deriv (multiplicand exp) var))
               (make-product
                 (deriv (multiplier exp) var)
                 (multiplicand exp))))

  (put 'deriv '+ sum-deriv)
  (put 'deriv '* mul-deriv)
)

; skip c
; d
; change order of key of table

;2.74
;a 


(define (attach-divison divison content) (cons divison content))
(define (divison table) (car table))
(define (contents table) (cadr table))

(define (apply-generic op name table)
  (let ((div (divison table)))
    (let ((proc (get op div)))
      (if proc
        (proc name table)
        (error "no operator" op)))))

(define (get-record name table)
  (apply-generic 'get-record name table))

(define (get-salary adr sal record)
  (apply-generic 'get-salary sal record))

(define (find-employee-record name all-tables)
  (if (null? all-table)
    '()
    (let ((cur-table (car all-tables)))
      (cond (get-record name cur-table)
            (find-employee-record name (cdr all-tables))))))
;d only add sum packages


;2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) z)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;2.76
; when new data type is added,
; 1) explicit dispatch
;    add new tag, add new code to procedure with different name(e.g. suffix with new data type)
;    last, put those procedures to general operation
; 2) data directed
;    make a set of new package, and put to tables. general operation handles new types by new records on table
; 3) message passing
;    define new make-* function. doesn't need table.

; when new operation is added,
; 1) explicit dispatch
;    make new procedures for each data types with different name + new general operation
; 2) data directed
;    put new prcedure to each package and put that to table. write 1 line deifiniton of new operation.
; 3) message passing
;    define new procedure in make-* function. doesn't need table.

;when there is a lot of new data types
;-> message passing

;when there is a lot of new operations
;-> data-directed

;2.77
; there is no op in tables.

;2.78
(define (attatch-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else error "Bad tagged datum: CONTENTS" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else error "Bad tagged datum: CONTENTS" datum)))

; skip 2.79 ~ 2.80

; 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))
; a
; complex->complex exists, but proc of exp with two complexs doesn't exists, so infinite loops

; b
; in this scenario, throwing error is better.

; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2)
              ; if types are same but didn't find proc, should throw error
              (error "No method for these types" (list op type-tags))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                      (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                      (else (error "No method for these types"
                                   (list op type-tags)))))))
          (error "No method for these types"
                 
; skip 2.82

; 2.83
(define (raise arg type-tower)
  (if (null? (cdr type-tower))
    arg
    (let ((type-arg (type-tag arg))
          (type-cur (type-tag (car type-tower)))
          (type-upper (type-tag (cadr type-tower))))
      (if (eq? type-arg type-cur)
       ((get-coercion type-arg type-upper) arg)
       (raise arg (cdr type-tower))))))

; skip 2.84
; 2.85
(define (drop num)
  (let ((project (get 'project (type-tag num)))
      (if project
        (let ((projected (project (contents num)))
          ; 값을 내려도, 원래의 값과 차이가 없다면(15+i를 15로 drop 할 수는 없음)
          (if (eq? num (raise projected))
            (drop projected)
            num))
        num))))

; skip 2.86
