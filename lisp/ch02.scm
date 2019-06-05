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
