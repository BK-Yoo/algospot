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
    (+ low (random (* 1.0 range)))))

(define (estimate-integral check x1 x2 y1 y2 trials)
  (define (iter passed tried)
    (let ((rand-x (random-in-range x1 x2))
          (rand-y (random-in-range y1 y2)))
      (cond ((= tried 0) (/ passed trials))
            ((check rand-x rand-y) (iter (+ passed 1) (- tried 1)))
            (else (iter passed (- tried 1))))))
  (* 1.0 (- x2 x1) (- y2 y1) (iter 0 trials))
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