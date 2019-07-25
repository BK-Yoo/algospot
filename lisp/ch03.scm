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