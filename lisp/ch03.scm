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

; 3.19

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

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
        (set-value! sum
          (+ (get-value a1) (get-value a2))
          me))
      ((and (has-value? a1) (has-value? sum))
        (set-value! a2
          (- (get-value sum) (get-value a1))
          me))
      ((and (has-value? a2) (has-value? sum))
        (set-value! a1
          (- (get-value sum) (get-value a2))
          me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or (and (has-value? m1) (= (get-value m1) 0))
         (and (has-value? m2) (= (get-value m2) 0)))
        (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
        (set-value! product
          (* (get-value m1) (get-value m2))
          me))
      ((and (has-value? product) (has-value? m1))
        (set-value! m2
          (/ (get-value product)
            (get-value m1)) me))
      ((and (has-value? product) (has-value? m2))
        (set-value! m1
          (/ (get-value product) (get-value m2))
          me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request)) (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

; setter and retractor must be a one of constraint(adder, multiplier ..)
; it means connector only changed by a constraint which set the value of that constraint
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
          (set! value newval)
          (set! informant setter)
          (for-each-except setter inform-about-value constraints))
        ((not (= value newval))
          (error "Contradiction" (list value newval)))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant false)
          (for-each-except retractor inform-about-no-value constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint)
          'done))
    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant true false))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        ((eq? request 'source) informant)
        (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
      ((eq? (car items) exception) (loop (cdr items)))
      (else (procedure (car items))
        (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
(define (source connector)
  (connector 'source))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
         (v (make-connector))
         (w (make-connector))
         (x (make-connector))
         (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
      'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)


; 3.33
; a + b = u -- u = c * 2
(define (averager a b c)
  (let ((u (make-connector))
         (x (make-connector)))
    (adder a b u)
    (multiplier c x u)
    (constant 2 x)
      'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "value A" a)
(probe "value B" b)
(probe "value C" c)

(set-value! a 20 'user)
(set-value! b 30 'user)

; 3.34
(define (wrong-squarer a b)
  (multiplier a a b))

; in multiplier,
; set value to e doesn't trigger the event
; because we need at least 2 connetors have value(in this case only e)

; 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0: SQUARER")
        (set-value! a (sqrt (get-value b)) me))
      (if (has-value? a)
        (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define d (make-connector))
(define e (make-connector))

(squarer d e)
(probe "value d" d)
(probe "value e" e)

(set-value! e 50 'user)

; 3.36 skip

; 3.37

(define (celsius-fahrenheit-converter2 c)
  (c+ (c* (c/ (cv 9) (cv 5)) c)
    (cv 32)))

(define (c+ a b)
  (let ((z (make-connector)))
    (adder a b z)
    z))

(define (c* a b)
  (let ((z (make-connector)))
    (multiplier a b z)
    z))

(define (c/ a b)
  (let ((z (make-connector)))
    (multiplier b z a)
    z))

(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))

(define X (make-connector))
(define Y (celsius-fahrenheit-converter2 X))

(probe "X: " X)
(probe "Y: " Y)

(set-value! X 30 'user)

; 3.38

(define balance 100)
(define (reset-balance) (set! balance 100))

; Peter
(define (peter) (set! balance (+ balance 10)))
; Paul
(define (paul) (set! balance (- balance 20)))
; Mary
(define (mary) (set! balance (- balance (/ balance 2))))

; a => 45, 35, 45, 50, 40, 40
(newline)
(begin (reset-balance) (peter) (paul) (mary) (display balance) (newline))
(begin (reset-balance) (peter) (mary) (paul) (display balance) (newline))
(begin (reset-balance) (paul) (peter) (mary) (display balance) (newline))
(begin (reset-balance) (paul) (mary) (peter) (display balance) (newline))
(begin (reset-balance) (mary) (paul) (peter) (display balance) (newline))
(begin (reset-balance) (mary) (peter) (paul) (display balance) (newline))

;b
; 55, if peter's transcation overlapped peter's then mary withdraw.

; 3.39
; (define x 10)
; (define s (make-serializer))
; (parallel-execute
;  (lambda () (set! x ((s (lambda () (* x x)))))
;  (s (lambda () (set! x (+ x 1))))))

; all the operations related with getting the value of x will be serialized
; 101: possible
; 121: possible
; 110: impossible
; 11: possible
; 100: possible

; 3.40
; (define x 10)
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))
; 10^2, 10^3, 10^4, 10^5, 10^6

; (parallel-execute (s (lambda () (set! x (* x x))))
;                   (s (lambda () (set! x (* x x x)))))
; 10^6

; 3.41
(define (make-serializer) (lambda (f) f))
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit) (protected deposit))
        ((eq? m 'balance) ((protected (lambda () balance))))
        (else (error "Unknown request: MAKE-ACCOUNT") m)))
    dispatch))

; while someone withdraw or deposit, the others can get wrong value
; but in this book, get balance doesn't change state so it's ok.

; 3.42
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      (error "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
           (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
          ((eq? m 'deposit) protected-deposit)
          ((eq? m 'balance) balance)
          (else (error "Unknown request: MAKE-ACCOUNT") m)))
      dispatch)))
; it's safe to change
