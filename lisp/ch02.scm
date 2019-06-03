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

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (car z)
  (z (lambda (p q) q)))

;2.5
(define (exp n p)
  (define (iter k result)
    (if (not (< 0 k))
      result
      (iter (- k 1) (* result n))))
  (iter p 1)
)

(define (cons a b) (* (exp 2 a) (exp 3 b))) 

(define (num-of-mul z divisor)
  (define (iter n k)
    (if (= (remainder n divisor) 0)
      (iter (/ n divisor) (+ k 1))
      k)
  )
  (iter z 0)
)

(define (car z)
  (num-of-mul z 2))

(define (cdr z)
  (num-of-mul z 3))

;2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
