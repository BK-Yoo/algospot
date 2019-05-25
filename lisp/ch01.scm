(define (square x)(* x x))
(define three 3)

(define (get-smallest x y z) (cond ((and (< x y) (< x z)) x)
                                  ((and (< y x) (< y z)) y)
                                  ((and (< z x) (< z y)) z)
				  (else x)
				  )
  )
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
  (define (good-enough? guess) (< (abs-diff (square guess) x) 0.001))
  (define (newton-method guess)
    (if (good-enough? guess)
      guess
      (newton-method (/ (+ guess (/ x guess)) 2))))
  (newton-method 1.0))


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
   (newton-method 1.0))


;1.1.8

(define (cube x)
  (define (improve guess) (/ (+ (/ x (* guess guess)) (* 2.0 guess)) 3.0))
  (define (good-enough? guess) (< (abs-diff (improve guess) guess) (* 0.001 guess)))
  (define (newton-method guess)
    (if (good-enough? guess)
      guess
      (newton-method (improve guess))))
  (newton-method 1.0))

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
        (* (/ (+ a 1.0) (+ a 2.0)) (product (next a) next b))
        (* (/ (+ a 2.0) (+ a 1.0)) (product (next a) next b))
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
              (* (/ (+ k 1.0) (+ k 2.0)) result)
              (* (/ (+ k 2.0) (+ k 1.0)) result)
            )
      )
    )
  )
  (iter a 1)
)
