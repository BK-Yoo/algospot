;3.1
(define (make-accumulator num)
  (let ((acc 0))
    (lambda (num)
      (begin (set! acc (+ acc num))
        acc)
    )
  )
)

;3.2
(define (make-monitored func)
  (let ((call-count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) call-count)
        ((eq? arg 'reset-count) (begin (set! call-count 0) 'reset))
        (else (begin (set! call-count (+ call-count 1)) (func arg)))
      )
    )
  )
)

;3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (> balance amount)
      (begin (set! balance (- balance amount)) balance)
      (error "Insufficient funds")
    )
  )
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (define (dispatch pwd cmd)
    (if (eq? pwd password)
      (cond ((eq? cmd 'withdraw) withdraw)
        ((eq? cmd 'deposit) deposit)
        (else (error "Unsupported method")))
      (error "Incorrect password")
    )
  )
  dispatch)

;3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (> balance amount)
      (begin (set! balance (- balance amount)) balance)
      (error "Insufficient funds")
    )
  )
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (let ((incorrect_cnt 0))
    (lambda (pwd cmd)
      (if (eq? pwd password)
        (cond ((eq? cmd 'withdraw) withdraw)
          ((eq? cmd 'deposit) deposit)
          (else (error "Unsupported method")))
        (begin (set! incorrect_cnt (+ incorrect_cnt 1))
          (if (> incorrect_cnt 6) (error "call the cops") (error "Incorrect password")))
      )
    )
  )
)

;3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (* 1 .0 range)))))

(define (estimate-integral check x1 x2 y1 y2 trials)
  (define (iter passed tried)
    (let ((rand-x (random-in-range x1 x2))
           (rand-y (random-in-range y1 y2)))
      (cond ((= tried 0) (/ passed trials))
        ((check rand-x rand-y) (iter (+ passed 1) (- tried 1)))
        (else (iter passed (- tried 1))))))
  (* 1 .0 (- x2 x1) (- y2 y1) (iter 0 trials))
)

(estimate-integral (lambda (x y) (>= 1 (+ (square x) (square y)))) -1 1 -1 1 1000) ; = pi

;3.6
(define random-init 4)
(define rand
  (let ((random-seed random-init))
    (define (rand-update seed) (modulo (* seed 31) 17))
    (define (dispatch command)
      (cond ((eq? command 'generate)
              (set! random-seed (rand-update random-seed)) random-seed)
        ((eq? command 'reset)
          (lambda (reset) (set! random-seed reset) random-seed))
        (else (error 'no-such-commmand)))) dispatch))

;3.7
(define (make-joint account root-pwd access-pwd)
  (if (number? ((account root-pwd 'deposit) 0))
    (lambda (pwd cmd)
      (if (eq? pwd access-pwd)
        (account root-pwd cmd)
        (error 'wrong-password)))
    (error 'wrong-password)))

;3.8
; (f 0) + (f 1) = 0
; (f 1) + (f 0) = 1
(define f
  (let ((property 0))
    (lambda (num)
      (define temp property)
      (set! property num)
      temp)))

;3.9
; iterative version uses more arguments, but both version uses smae number of env

;3.10
; body includes parameter(in this case, initial-abmount),
; so in define phase, two envs are created, one for initial-amount and the other for balance
; in set! phase, env whose target is balance is changed directly to 50, but former one doesn't be affected

;3.11
; global = [acc, make-account]
; E1 = [balance: 50, withdraw, deposit, dispatch]
; E2(withdraw)
; E3(deposit)
; E4(dispatch)

;((acc 'depoist) 40)
; E4 = [m: 'deposit]
; E3 = [amount: 40]
; E1 = [balance: 90]

;((acc 'withdraw) 60)
; E4 = [m: 'withdraw]
; E2 = [amount: 60]
; E1 = [balance: 30]

; acc and acc2 don't share local memory but global.

;3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

; response1 -> b
; response2 -> b c d
; because x is set by set-cdr! in append!

;3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
; |<---------|
; (a -> b -> c)

;3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

; (a b c d) ()
; (b c d) (a)
; (c d) (b a)
; (d) (c b a)
; () (d c b a)
; mystery = in-place reverse function

;3.15 - skip

;3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
      (count-pairs (cdr x))
      1)))

(count-pairs (list 'a 'b 'c)) ;3
(count-pairs (list (list 'a 'b) 'd)) ;4
(count-pairs (list (list 'a 'b 'c) (list 'd 'e))) ;7
; (count-pairs (make-cycle (list 'a 'b 'c))) ;infinite

;3.17
(define (already-search e history)
  (if (null? history)
    false
    (let ((temp (car history)))
      (if (eq? e temp)
        true
        (already-search e (cdr history))))))

(define (count-pairs-fix x)
  (define (count-pairs e history)
    (if (or (not (pair? e)) (already-search e history))
      0
      (+ (count-pairs (car e) (cons e history))
        (count-pairs (cdr e) (cons e history))
        1)))
  (count-pairs x '()))

(count-pairs-fix (make-cycle (list 'a 'b 'c)))

;3.18
(define (has-cycle x)
  (let ((history '()))
    (define (iter e)
      (cond ((not (pair? e)) false)
        ((already-search e history) true)
        (else (begin
                (set! history (cons e history))
                (or (iter (car e))
                  (iter (cdr e)))))))
    (iter x)))

(has-cycle (make-cycle (list 'a 'b 'c)))

; 3.18

; memory increases linearly by one function call
(define (has-cycle-fix x)
  (if (not (pair? x))
    false
    (let ((first-element (car x)))
      (define (iter e)
        (if (already-search first-element (cdr e))
          true
          (or (has-cycle-fix (car e))
            (has-cycle-fix (cdr e)))))
      (iter x))))

(define (has-cycle-fix x)
  (define (check slow fast)
    (cond ((not (pair? slow)) false)
      ((not (pair? fast)) false)
      ((or (null? (cdr slow)) (null? (cdr fast)) (null? (cddr fast))) false)
      ((eq? slow fast) true)
      (else (check (cdr slow) (cddr fast)))))
  (if (or (not (pair? x)) (null? (cdr x)) (not (pair? (cdr x))) (null? (cddr x)))
    false
    (check (cdr x) (cddr x)))
)

; community answer 1 -> this one is correct
(define (contains-cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
      (cdr l)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
      ((not (pair? b)) #f)
      ((eq? a b) #t)
      ((eq? a (safe-cdr b)) #t)
      (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

; community answer 2 -> makes error when x is not pair
(define (has-loop? x)
  (define (check slow fast)
    (cond ((eq? slow fast) #t)
      ((or (null? (cdr fast)) (null? (cddr fast))) #f)
      (else (check (cdr slow) (cddr fast)))))
  (check x (cdr x)))

(define z (make-cycle (list 'a 'b 'c)))
(define u (make-cycle (list 'a 'b 'c 'd 'e 'f)))
(define w (make-cycle (list 'a 'b 'c 'd 'e 'f 'g)))
(define t (make-cycle (list 'a 'b 'c 'd 'e 'f 'g 'h)))
(define v (make-cycle (list 'a 'b)))
(define x (make-cycle (list 'a)))
(define y (make-cycle (list '())))
(define a '())
(define b (list 'a))
(define c (list 'a 'b))
(define d (list 'a 'b 'c))

(display (has-cycle-fix z))
(display (has-cycle-fix u))
(display (has-cycle-fix w))
(display (has-cycle-fix t))
(display (has-cycle-fix v))
(display (has-cycle-fix x))
(display (has-cycle-fix y))
(display (has-cycle-fix a))
(display (has-cycle-fix b))
(display (has-cycle-fix c))
(display (has-cycle-fix d))