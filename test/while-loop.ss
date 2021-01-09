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
       (with-syntax
         ((break (datum->syntax (syntax while) 'break))
          (continue (datum->syntax (syntax while) 'continue)))
         (syntax
           (call/cc
             (lambda (break)
               (let continue ()
                 (when condition body ... (continue)))))))))))

#;#;#;
(print-gensym #f)
(pretty-print
  (expand/optimize
    '(define (sum-while-syntax m)
      (let ([i 1] #|  |# [s 0])
        (while (<= i m)
               (when (odd? i)
                     (inc! i)
                     (continue))
               (inc! s i)
               (inc! i))
        s))))

(let ([i 0][m 0] ;
      )
  (while (< i 100)
         (when (< i m) (inc! i) (continue))
         (when (> i 50)
               (printf "terminal at ~a\n" i)
               (break))
         (let ([j 1])
           (while (<= j i)
                  (printf "~a" j)
                  (set! j (* j 2))
                  (if (<= j i)
                      (printf " ")
                      (printf ", " ))))
         (printf "~a\n" i)
         (set! m (* 2 i))
         (inc! i))#|  |#)
; #!eof
(define (sum-while-syntax m)
  (let ([i 1][s 0])
    (while (<= i m)
           (inc! s i)
           (inc! i))
    s))
(define (sum-let-loop-with-set m)
  (let ([i 1][s 0])
    (let loop ()
      (when (<= i m)
            (inc! s i)
            (inc! i)
            (loop)))
    s))
(define (sum-let-loop-without-set m)
  (let while ([i 1][s 0])
    (if (<= i m)
        (while (+ i 1) (+ s i))
        s)))

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
