(define (square x) (* x x))
(define three 3)

(define (get-smallest x y z) (cond ((and (< x y) (< x z)) x)
                               ((and (< y x) (< y z)) y)
                               ((and (< z x) (< z y)) z)
                               (else x)))
(define (square x) (* x x))
(define (calculate x y z) (+ (square (if (= x (get-smallest x y z))
                                       0
                                       x))
                            (square (if (= y (get-smallest x y z))
                                      0
                                      y))
                            (square (if (= z (get-smallest x y z))
                                      0
                                      z))
                          ))

(define (abs-diff x y) (abs (- x y)))


;1.1.5
(define (sqrt x)
  (define (good-enough? guess) (< (abs-diff (square guess) x) 0 .001))
  (define (newton-method guess)
    (if (good-enough? guess)
      guess
      (newton-method (/ (+ guess (/ x guess)) 2))))
  (newton-method 1 .0))


;1.1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
    (else else-clause)))

;1.1.7

;if there are too many steps to reach machine-preicision, it takes too much time.
(define (sqrt x)
  (define (improve guess) (/ (+ guess (/ x guess)) 2))
  (define (good-enough? guess) (= (improve guess) guess))
  (define (newton-method guess)
    (if (good-enough? guess)
      guess
      (newton-method (improve guess))))
  (newton-method 1 .0))


;1.1.8

(define (cube x)
  (define (improve guess) (/ (+ (/ x (* guess guess)) (* 2 .0 guess)) 3 .0))
  (define (good-enough? guess) (< (abs-diff (improve guess) guess) (* 0 .001 guess)))
  (define (newton-method guess)
    (if (good-enough? guess)
      guess
      (newton-method (improve guess))))
  (newton-method 1 .0))

; factorial - recursive
(define (factorial_re n)
  (if (<= n 0)
    1
    (* n (factorial_re (- n 1)))
  )
)

; factorial - iterative
(define (factorial_it n)
  (define (iter counter product)
    (if (> counter n)
      product
      (iter (+ 1 counter) (* counter product))))
  (iter 1 1)
)


;1.11

; recursive
(define (prac n)
  (if (< n 3)
    n
    (+ (prac (- n 1)) (* 2 (prac (- n 2))) (* 3 (prac (- n 3))))
  )
)

(define (prac_it n)
  (define (prac n_1 n_2 n_3 n)
    (if (= n 0)
      n_3
      (prac (+ n_1 (* 2 n_2) (* 3 n_3)) n_1 n_2 (- n 1))
    )
  )
  (prac 2 1 0 n)
)


;1.29
(define (cube x) (* x x x))
(define (simpson f a b n)
  (define (iter fk i n result)
    (if (> i n)
      result
      (iter fk (+ 1 i) n
        (+ result
          (* (fk i)
            (cond ((= i 0) 1)
              ((= i n) 1)
              ((= (modulo i 2) 0) 2)
              ((= (modulo i 2) 1) 4)
            )
          )
        )
      )
    )
  )
  (let ((h (/ (- b a) n)))
    (/ (* h (iter (lambda (k) (f (+ a (* k h)))) 0 n 0)) 3)
  )
)

;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ a result))
    )
  )
  (iter a 0)
)

;1.31
(define (product-re a next b)
  (define (odd? num) (= (modulo num 2) 1))
  (define (iter a next b)
    (if (> a b)
      1
      (if (odd? a)
        (* (/ (+ a 1 .0) (+ a 2 .0)) (product (next a) next b))
        (* (/ (+ a 2 .0) (+ a 1 .0)) (product (next a) next b))
      )
    )
  )
  (* 4 (iter a next b))
)

(define (product-it a next b)
  (define (odd? num) (= (modulo num 2) 1))
  (define (iter k result)
    (if (> k b)
      (* 4 result)
      (iter (next k)
        (if (odd? k)
          (* (/ (+ k 1 .0) (+ k 2 .0)) result)
          (* (/ (+ k 2 .0) (+ k 1 .0)) result)
        )
      )
    )
  )
  (iter a 1)
)

;1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-values (next a) next b))
  )
)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))
    )
  )
  (iter a null-value)
)

;1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (combiner (if (filter a) (term a) null-value)
      (filtered-accumulate combiner null-value (next a) next b filter))
  )
)


(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter k result)
    (if (> k b)
      result
      (iter (next k)
        (combiner (if (filter k) (term k) null-value) result))
    )
  )
  (iter a null-value)
)

;a
(define (smallest-divisor n)
  (define (divides? n divisor) (= (remainder n divisor) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
      ((divides? n test-divisor) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))
    )
  )
  (find-divisor n 2)
)

(define (prime-sum term a next b)
  (filtered-accumulate + 0 term a next b (lambda (n) (and (not (= n 1)) (= (smallest-divisor n) n))))
)

;b
(define (relative-prime-sum term a next b)
  (filtered-accumulate * 1 term a next b (lambda (n) (= (gcd n b) 1)))
)

;1.35
(define tolerance 0 .00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess)
)

(define (get-golden-ratio)
  (fixed-point (lambda (x) (+ 1 .0 (/ 1 .0 x))) 1 .0))

;1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess)
)

(define (approx)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
)

(define (approx-with-avg)
  (let ((average (lambda (a b) (/ (+ a b) 2))))
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))
)

;1.37
(define (cont-frac n d k)
  (if (= 0 k)
    0
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))
  )
)

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))
    )
  )
  (iter k 0)
)

;1.38
(define (approx-e k)
  (+ 2
    (cont-frac (lambda (i) 1 .0)
      (lambda (i)
        (if (= (modulo i 3) 2)
          (* 2 (/ (+ i 1) 3))
          1
        )
      )
      k))
)

;1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
    (lambda (i) (- (* 2 .0 i) 1 .0))
    k))

;1.40
(define dx 0 .00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;1.41
(define (inc x) (+ 1 x))
(define (double f) (lambda (x) (f (f x))))

;1.42
(define (compose f g) (lambda (x) (f (g x))))

;1.43
(define (repeated f n)
  (if (= n 1)
    (lambda (x) (f x))
    (lambda (x) ((repeated f (- n 1)) (f x)))
  )
)

;1.44
(define (average a b c) (/ (+ a b c) 3 .0))
(define (smooth f) (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))
(define n-fold-smooth (lambda (n x) ((repeated smooth n) f) x))

;1.45
(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2 .0)))

(define (nth-root n)
  (define (n-mul n y)
    (if (= n 1)
      y
      (* y (n-mul (- n 1) y)))
  )
  (lambda (x)
    ; damp count can be optimized
    (fixed-point ((repeated average-damp n)
                   (lambda (y) (/ x (n-mul (- n 1) y))))
      1 .0)
  )
)

;1.46
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter x)
      (if (good-enough? x)
        x
        (iter (improve x))))
    (iter x))
)

(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess) (< (abs (- (f guess) guess)) tolerance))
     f) first-guess)
)

(define (sqrt x)
  ((iterative-improve
     (lambda (guess) (< (abs (- (square guess) x)) 0 .001))
     (lambda (guess) (/ (+ guess (/ x guess)) 2 .0))
   ) 1 .0)
)
