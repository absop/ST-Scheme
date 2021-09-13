(define-syntax quasiquote
  (let ()
    (define (quasi p lev)
      (syntax-case p (unquote quasiquote)
        [(unquote p)
         (if (= lev 0)
             #'("value" p)
             (quasicons #'("quote" unquote) (quasi #'(p) (- lev 1))))]
        [(quasiquote p) (quasicons #'("quote" quasiquote) (quasi #'(p) (+ lev 1)))]
        [(p . q)
         (syntax-case #'p (unquote unquote-splicing)
           [(unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                  (quasi #'q lev)))]
           [(unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote-splicing) (quasi #'(p ...) (- lev 1)))
                  (quasi #'q lev)))]
           [_ (quasicons (quasi #'p lev) (quasi #'q lev))])]
        [#(x ...) (quasivector (vquasi #'(x ...) lev))]
        [#&x (quasibox (quasi #'x lev))]
        [p #'("quote" p)]))
    (define (vquasi p lev)
      (syntax-case p ()
        [(p . q)
         (syntax-case #'p (unquote unquote-splicing)
           [(unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                  (vquasi #'q lev)))]
           [(unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                  (quasicons
                    #'("quote" unquote-splicing)
                    (quasi #'(p ...) (- lev 1)))
                  (vquasi #'q lev)))]
           [_ (quasicons (quasi #'p lev) (vquasi #'q lev))])]
        [() #'("quote" ())]))
    (define (quasicons x y)
      (with-syntax ([x x] [y y])
        (syntax-case #'y ()
          [("quote" dy)
           (syntax-case #'x ()
             [("quote" dx) #'("quote" (dx . dy))]
             [_ (if (null? #'dy) #'("list" x) #'("list*" x y))])]
          [("list" . stuff) #'("list" x . stuff)]
          [("list*" . stuff) #'("list*" x . stuff)]
          [_ #'("list*" x y)])))
    (define (quasiappend x y)
      (syntax-case y ()
        [("quote" ())
         (cond
           [(null? x) #'("quote" ())]
           [(null? (cdr x)) (car x)]
           [else (with-syntax ([(p ...) x]) #'("append" p ...))])]
        [_
         (cond
           [(null? x) y]
           [else (with-syntax ([(p ...) x] [y y]) #'("append" p ... y))])]))
    (define (quasilist* x y)
      (let f ((x x))
        (if (null? x)
            y
            (quasicons (car x) (f (cdr x))))))
    (define (quasivector x)
      (syntax-case x ()
        [("quote" (x ...)) #'("quote" #(x ...))]
        [_
         (let f ([y x] [k (lambda (ls) #`("vector" #,@ls))])
           (syntax-case y ()
             [("quote" (y ...)) (k #'(("quote" y) ...))]
             [("list" y ...) (k #'(y ...))]
             [("list*" y ... z) (f #'z (lambda (ls) (k (append #'(y ...) ls))))]
             [else #`("list->vector" #,x)]))]))
    (define (quasibox x)
      (syntax-case #'x ()
        [("quote" x) #'("quote" #&x)]
        [else #`("box" #,x)]))
    (define (emit x)
      (syntax-case x ()
        [("quote" x) #''x]
        [("list" x ...) #`(list #,@(map emit #'(x ...)))]
        [("list*" x y) #`(cons #,(emit #'x) #,(emit #'y))]
        [("list*" x ...) #`(list* #,@(map emit #'(x ...)))]
        [("append" x ...) #`(append #,@(map emit #'(x ...)))]
        [("vector" x ...) #`(vector #,@(map emit #'(x ...)))]
        [("list->vector" x) #`(list->vector #,(emit #'x))]
        [("box" x) #`(box #,(emit #'x))]
        [("value" x) #'x]))
    (lambda (x)
      (syntax-case x ()
       ; convert to intermediate language, combining introduced (but not
       ; unquoted source) quote expressions where possible and choosing
       ; optimal construction code otherwise, then emit Scheme code
       ; corresponding to the intermediate language forms.
        [(_ e) (emit (quasi #'e 0))]))))
