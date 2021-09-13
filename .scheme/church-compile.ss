(load "match.ss")


(define (foldl op init xs) (fold-left (lambda (acc x) (op x acc)) init xs))
(define (foldr op init xs) (fold-right op init xs))

;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)
;     | (let ([x e] ...) e)
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)
;     | x
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f
; nat ::= 0 | 1 | 2 | ...
; x is a symbol
; prim is a primitive operation in list prims

(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;

(define (churchify e)
  (define (syntax-error e)
    (error 'churchify "syntax error at: ~a" e))

  (define (currify xs body)
    (foldr (lambda (x body) `(lambda (,x) ,body))
           (churchify body)
           xs))

  (define (currify* f es)
    (foldl (lambda (e f) `(,f ,(churchify e))) f es))

  (define (xlet xs es body) (currify* (currify xs body) es))

  (define (xlet* xs es body)
    (define (bind* x e body)
      `((lambda (,x) ,body) ,(churchify e)))
    (foldr bind* (churchify body) xs es))

  (define (xletrec f e body)
    (match e
      [(lambda (,(symbol? xs) ...) ,exp)
       `((lambda (,f) ,(churchify body))
         (Y (lambda (,f) ,(currify xs exp))))]
      [_ (syntax-error `(letrec ([,f ,e] ,body)))]))

  (define id `(lambda (x) x))
  (define true  `(lambda (T) (lambda (F) (T ,id))))
  (define false `(lambda (T) (lambda (F) (F ,id))))

  (define (if** test then else)
    `((,test (lambda (_) ,then)) (lambda (_) ,else)))

  (define (if* test then else)
    (if** (churchify test) (churchify then) (churchify else)))

  (define (and* es r)
    (foldr (lambda (e later) (if** (churchify e) later false)) r es))

  (define (or* es r)
    (foldr (lambda (e later) (if** (churchify e) true later)) r es))

  (define (nat->church n)
    `(lambda (f)
       (lambda (z)
         ,(foldr (lambda (_ n) `(f ,n)) 'z (iota n)))))

  (match e
    [(lambda (,(symbol? xs) ...) ,body) (currify xs body)]
    [(letrec ([,(symbol? x) ,e]) ,body) (xletrec x e body)]
    [(let ([,(symbol? xs) ,es] ...) ,body) (xlet xs es body)]
    [(let* ([,(symbol? xs) ,es] ...) ,body) (xlet* xs es body)]
    [(if ,test ,then ,else) (if* test then else)]
    [(and ,es ...) (and* es true)]
    [(or ,es ...) (or* es false)]
    [(quote ()) false]
    [(,ef ,es ...) (currify* (churchify ef) es)]
    [,(integer?) (guard (not (negative? e))) (nat->church e)]
    [,(symbol?) e]
    [#t true]
    [#f false]
    [_ (syntax-error e)]))

(define (church-compile program)
  (define sub1
    '(lambda (n f z)
       (((n (lambda (g) (lambda (h) (h (g f)))))
         (lambda (u) z))
        (lambda (u) u))))
  (define zero? `(lambda (n) ((n (lambda (x) #f)) #t)))
  (define - `(lambda (n m) ((m ,sub1) n)))
  (churchify
   `(let ([add1 (lambda (n f z) (f ((n f) z)))]
          [sub1 ,sub1]
          [zero? (lambda (n) ((n (lambda (x) #f)) #t))]
          [+ (lambda (n m f z) ((n f) ((m f) z)))]
          [- (lambda (n m) ((m ,sub1) n))]
          [* (lambda (n m f z) ((n (m f)) z))]
          [= (lambda (n m) (and (,zero? (,- n m)) (,zero? (,- m n))))]
          [not (lambda (b) (if b #f #t))]
          [cons (lambda (x y c _) ((c x) y))]
          [car (lambda (p) ((p (lambda (x y) x)) (lambda (x) x)))]
          [cdr (lambda (p) ((p (lambda (x y) y)) (lambda (x) x)))]
          [pair? (lambda (p) ((p (lambda (x y) #t)) (lambda (_) #f)))]
          [null? (lambda (p) ((p (lambda (x y) #f)) (lambda (_) #t)))]
          [Y (lambda (f)
               ((lambda (u) (u u))
                (lambda (x) (f (lambda (y) ((x x) y))))))])
      ,program)))


(define (church->nat c-nat)
  ((c-nat add1) 0))

(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))



(define prog
  (church-compile
    '(if (= (+ 3 3) (* 2 3))
         (- 5 1)
         (let ([U (lambda (u) (u u))])
           (U U)))))

(pretty-print prog)
(printf "~a\n" (church->nat (eval prog)))
