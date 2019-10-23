;(define (delay exp) (lambda () exp))
;(define (force delayed-object) (delayed-object))
;(define (cons-stream a b) (cons a (delay b)))
;(define (stream-car stream) (car stream))
;(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    the-empty-stream
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (memo-proc proc)
  (let ((already-run? false)
         (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? true)
               result)
        result))))

; delay can be "(memo-proc (lambda () <exp>))"

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map (cons proc (map stream-cdr argstreams))))))

; 3.51
(define (show x) (display-line x) x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
; default delay save the result of expresison

; 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(display-stream z)
; cause result is memoized, it can be free to effect of assignment.
; but if we don't memoize, sum would be added twice.

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; (stream-cdr integers)
; = (add-streams ones integers)
; = (2 (add-streams (stream-cdr ones) (stream-cdr integers)))
; = (2 (add-streams ones (add-streams ones integers)))

; (stream-cdr (stream-cdr integers)
; = (add-streams ones (add-streams ones integers))
; = (add-streams ones (2 (add-streams ones (add-streams ones integers))))
; = (3 (add-streams ones (add-streams ones (add-streams ones integers))))

; (stream-cdr (stream-cdr (stream-cdr integers)))
; = (add-streams ones (add-streams ones (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

; (stream-cdr fibs)
; = (cons-stream 1 (add-streams (stream-cdr fibs) fibs))

; (stream-cdr (stream-cdr fibs))
; = (add-streams (stream-cdr fibs) fibs)
; = (add-streams (cons-stream 1 (add-streams (stream-cdr fibs) fibs)) fibs)
; = (cons-stream (+ 1 0)
;                (add-streams (add-streams (stream-cdr fibs) fibs) (stream-cdr fibs)))
; = (cons-stream 1 (add-streams (stream-cdr (stream-cdr fibs)) (stream-cdr fibs))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
      ((divisible? n (stream-car ps)) false)
      (else (iter (stream-cdr ps)))))
  (iter primes))

; 3.53
(define s (cons-stream 1 (add-streams s s)))
(define ss (cons-stream 1 (scale-stream ss 2)))

; (stream-cdr s)
; = (add-streams s s)
; = (2 (add-streams (stream-cdr s) (stream-cdr s))

; (stream-cdr (stream-cdr s))
; = (add-streams (stream-cdr s) (stream-cdr s))
; = (add-streams (2 (add-streams (stream-cdr s) (stream-cdr s))) (2 (add-streams (stream-cdr s) (stream-cdr s))))
; = (4 (add-streams (stream-cdr (stream-cdr s)) (stream-cdr (stream-cdr s))))

; 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 1) factorials)))

; (stream-cdr factorials)
; = (1 (mul-streams (stream-cdr (integers-starting-from 1)) (stream-cdr factorials)))
; = (1 (mul-streams (integers-starting-from 2) (stream-cdr factorials)))

; (stream-cdr (stream-cdr factorials))
; = (mul-streams (integers-starting-from 2) (stream-cdr factorials)))
; = (mul-streams (integers-starting-from 2) (1 mul-streams (integers-starting-from 2) (stream-cdr factorials))))
; = (2 (mul-streams (integers-starting-from 3) (mul-streams (integers-starting-from 2) (stream-cdr factorials))))

; 3.55
(define (partial-sum stream)
  (define x (cons-stream (stream-car stream) x))
  (cons-stream 0 (add-streams x (partial-sum (stream-cdr stream)))))

(define (partial-sum stream)
  (add-streams stream (cons-stream 0 (partial-sum stream))))
; (add-stream (stream-cdr stream) (stream-cdr (partial-sum stream)))

(define (partial-sum2 stream)
  ; (cons-stream 0 ~  => can't be answer becuase, first element should be s0
  (cons-stream (stream-car stream)
    (add-streams (stream-cdr stream) (partial-sum2 stream))))

; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ((s1car (stream-car s1))
             (s2car (stream-car s2)))
        (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
          ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
          (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; 3.57
; it's like tree, A(n) = A(n-1) + A(n-2), branch grows exponentially

; 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define expand-1710 (expand 1 7 10)) ; (/ 1.0 7)
(define expand-3810 (expand 3 8 10)) ; (/ 3.0 8)

; 3.59

; a
(define (integrate-series a-stream)
    (mul-streams (stream-map / ones integers) a-stream))

; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-sereis (scale-stream sine-series -1))))

; 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                                     (mul-series (stream-cdr s1) s2))))

; 3.61
; this procedure doesn't memoize the result of invert-unit-series
(define (invert-unit-series s1)
  (cons-stream 1
    (scale-stream (mul-series (stream-cdr s1) (invert-unit-series s1)) -1)))

; 3.62
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

; tangent
(define tans-series (div-series sine-series cosine-series))