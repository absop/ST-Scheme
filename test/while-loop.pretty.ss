(define-syntax inc!
  (syntax-rules ()
    [(_ x) (set! x (+ x 1))]
    [(_ x d) (set! x (+ x d))]))

(define-syntax dec!
  (syntax-rules ()
    [(_ x) (set! x (- x 1))]
    [(_ x d) (set! x (- x d))]))

(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((while condition body ...)
       (with-syntax ((break (datum->syntax (syntax while) 'break))
                     (continue (datum->syntax (syntax while) 'continue)))
         (syntax (call/cc
                   (lambda (break)
                     (define continue #f)
                     (call/cc
                       (lambda (k) (set! continue k)))
                     (let loop ()
                       (when condition body ... (loop)))))))))))


(let ([v '#(1 2 3 4 5 6 7 8 9 10)])
  (let-syntax ([get (syntax-rules () [(_ i) (vector-ref v i)])])
    (printf "(get 3) = ~a\n" (get 3))))


(let ([v '#(1 2 3 4 5 6 7 8 9 10)])
  (define-syntax get
    (syntax-rules () [(_ v i) (vector-ref v i)]))
  (printf "(get 3) = ~a\n" (get v 3)))


#;#;#;
(print-gensym #f)
(pretty-print
  (expand/optimize
    '(define (sum-while-syntax m)
       (let ([i 1] #|  |# [s 0])
         (while (<= i m)
           (when (odd? i) (inc! i) (continue))
           (inc! s i)
           (inc! i))
         s))))

(let ([i 0] [m 0] ;
      )
  (while (< i 100)
    (when (< i m) (inc! i) (continue))
    (when (> i 50) (printf "terminal at ~a\n" i) (break))
    (let ([j 1])
      (while (<= j i)
        (printf "~a" j)
        (set! j (* j 2))
        (if (<= j i) (printf " ") (printf ", "))))
    (printf "~a\n" i)
    (set! m (* 2 i))
    (inc! i))
  #|  |#
  )
; #!eof
(define (sum-while-syntax m)
  (let ([i 1] [s 0]) (while (<= i m) (inc! s i) (inc! i)) s))
(define (sum-let-loop-with-set m)
  (let ([i 1] [s 0])
    (let loop () (when (<= i m) (inc! s i) (inc! i) (loop)))
    s))
(define (sum-let-loop-without-set m)
  (let while ([i 1] [s 0])
    (if (<= i m) (while (+ i 1) (+ s i)) s)))

(collect)
(let ([s 0])
  (time (set! s (sum-while-syntax 100000000)))
  (printf "~a\n" s))
(collect)
(let ([s 0])
  (time (set! s (sum-let-loop-with-set 100000000)))
  (printf "~a\n" s))
(collect)
(let ([s 0])
  (time (set! s (sum-let-loop-without-set 100000000)))
  (printf "~a\n" s))