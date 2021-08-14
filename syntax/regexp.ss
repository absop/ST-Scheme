(let ()
  (define (string-join sep strs)
    (if (not (null? strs))
        (fold-left
          (lambda (acc s) (string-append acc sep s))
          (car strs)
          (cdr strs))
        ""))

  (define (string-prefix? sub sup)
    (let ([sublen (string-length sub)]
          [suplen (string-length sup)])
      (and (<= sublen suplen)
           (let eq-at? ([i 0])
             (or (and (< i sublen)
                      (char=? (string-ref sup i) (string-ref sub i))
                      (eq-at? (+ i 1)))
                 (= i sublen))))))

  (define (define? str)
    (or (string-prefix? "define" str)
        (string-prefix? "trace-define" str)))

  (let ([syms (environment-symbols (scheme-environment))])
    (let-values ([(vars syns) (partition top-level-bound? syms)])
      (let ([vars (map symbol->string vars)]
            [syns (map symbol->string syns)])
        (let-values ([(defs syns) (partition define? syns)])
          (display
            (string-join "\n"
              (map (lambda (strs) (string-join " " strs))
                   (list vars defs syns))))))))
)
