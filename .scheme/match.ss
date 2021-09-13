;; Pattern matching for list
;; p ::= _
;;     | ()
;;     | ,x
;;     | ,(sat? x)
;;     | ,(sat?)
;;     | (tag ,x y ...)    -- record-type
;;     | (pcar ... . rest)
;;     | (pcar . pcdr)
;;     | symbol
;;     | datum

(define-syntax (match x)
  (define lookup)

  (define (ellipsis? x)
    (and (identifier? x) (free-identifier=? x #'(... ...))))

  (define (wildcard? x)
    (and (identifier? x) (free-identifier=? x #'_)))

  (define (id=? x) (lambda (y) (free-identifier=? x y)))

  (define (record-type-name? name)
    (and (identifier? name)
         (let ([info (lookup name)])
           (and (pair? info)
                (memq (car info)
                  '(#{record val9xfsq6oa12q4-a}
                    #{r6rs-record vc7pishgmrh09qm-a}))))))

  (define (bind-once x v binds k)
    (if (assp (id=? x) binds)
        #`(if (equal? #,v #,x)
              #,(k binds)
              #f)
        #`(let ([#,x #,v])
            #,(k (cons `(,x ,v) binds)))))

  (define (split-new-binds xs ls binds)
    (let f ([xs xs] [ls ls] [nbs '()] [eqs '()])
      (if (null? xs)
          (values (reverse nbs) (reverse eqs))
          (let ([x (car xs)] [xs (cdr xs)]
                [l (car ls)] [ls (cdr ls)])
            (if (assp (id=? x) binds)
                (f xs ls nbs (cons #`(equal? #,l #,x) eqs))
                (f xs ls (cons `(,x ,l) nbs) eqs))))))

  (define (parse-record p v binds k)
    (with-syntax ([(rtn . fds) p] [p p] [v v])
      (with-syntax ([rtd #'(record-type-descriptor rtn)]
                    [vfi (gensymmap "~a" "vfi")])
        (define ?fis '())
        (define ?fi->fi
          (lambda (?fi)
            (or (and (number? ?fi) ?fi)
                #`(fn->fi '#,(datum->syntax #'* ?fi)))))
        (define (direct-access i) #`((record-accessor rtd #,i) v))
        (define (indirect-access i) (direct-access #`(vector-ref vfi #,i)))
        (define (parse-fields i fds fns binds)
          (syntax-case fds (unquote)
            [()
             (begin (set! ?fis (reverse fns)) (k binds))]
            [(,fn . fds)
             (or (identifier? #'fn)
                 (errorf 'match
                   "unquote non-identifier ~a in the record pattern ~a"
                   (datum fn) (datum p)))
             (bind-once #'fn (indirect-access i) binds
               (lambda (binds)
                 (parse-fields (+ i 1) #'fds (cons (datum fn) fns) binds)))]
            [(val . fds) (wildcard? #'val)
             (parse-fields (+ i 1) #'fds (cons i fns) binds)]
            [(val . fds)
             #`(if (equal? #,(direct-access i) val)
                   #,(parse-fields (+ i 1) #'fds (cons i fns) binds)
                   #f)]))
        (with-syntax ([code (parse-fields 0 #'fds '() binds)])
          (check-unique ?fis (datum p))
          #`(let* ([vfn (record-type-field-names rtd)]
                   [len (vector-length vfn)])
              (define (fn->fi fn)
                (or (let index ([i 0])
                      (and (< i len)
                           (or (and (eq? fn (vector-ref vfn i)) i)
                               (index (+ i 1)))))
                    (errorf 'match
                      "~a is not a field of the record type ~a" fn 'rtn)))
              (if ((record-predicate rtd) v)
                  (if (< len #,(length #'fds))
                      (errorf 'match
                        "too many fields in the record pattern ~a" 'p)
                      #,(with-syntax ([(fi ...) (map ?fi->fi ?fis)])
                          #`(let ([fis (list fi ...)])
                              (check-unique fis 'p)
                              #,(or (and (andmap number? ?fis) #'code)
                                  #'(let ([vfi (list->vector fis)])
                                     code)))))
                  #f))))))

  (define (parse-ellipsis p v binds k)
    (with-syntax ([(pcar rest) p] [v v]
                  [(vcar vcdr) (gensymmap "~a" '("vcar" "vcdr"))]
                  [(pair next) (gensymmap "~a" '("pair" "next"))])
      (let ([xs '()] [ls '()])
        (with-syntax
          ([code (parse-pattern #'pcar #'vcar '()
                   (lambda (binds)
                     (set! xs (map car binds))
                     (set! ls (gensymmap "~a*" (syntax->datum xs)))
                     (with-syntax ([((x l) ...) (map list xs ls)])
                       #'(next (cdr pair) (cons x l) ...))))])
          (let-values ([(nbs eqs) (split-new-binds xs ls binds)])
            (let* ([binds (append nbs binds)]
                   [ctn #`(let #,nbs #,(parse-pattern #'rest #'pair binds k))])
              (with-syntax ([(l ...) ls])
                #`(let next ([pair v] [l '()] ...)
                    (or (and (pair? pair)
                             (let ([vcar (car pair)])
                               code))
                        (let ([l (reverse l)] ...)
                          #,(if (null? eqs)
                                ctn
                                #`(if (and #,@eqs)
                                      #,ctn
                                      #f))))))))))))

  (define (parse-pattern p v binds k)
    (with-syntax ([v v])
      (syntax-case p (unquote)
        [p (wildcard? #'p) (k binds)]
        [() #`(if (null? v) #,(k binds) #f)]
        [,x (identifier? #'x) (bind-once #'x #'v binds k)]
        [,(sat?) #`(if (sat? v) #,(k binds) #f)]
        [,(sat? x) (identifier? #'x)
         #`(if (sat? v) #,(bind-once #'x #'v binds k) #f)]

        [(rtn fds ...) (record-type-name? #'rtn)
         (parse-record #'(rtn fds ...) #'v binds k)]

        [(pcar ?ell . rest) (ellipsis? #'?ell)
         (parse-ellipsis #'(pcar rest) #'v binds k)]

        [(pcar . pcdr)
         (with-syntax ([(vcar vcdr) (gensymmap "~a" '("vcar" "vcdr"))])
           #`(if (pair? v)
                 (let ([vcar (car v)] [vcdr (cdr v)])
                   #,(parse-pattern #'pcar #'vcar binds
                       (lambda (binds)
                         (parse-pattern #'pcdr #'vcdr binds k))))
                 #f))]

        [p (identifier? #'p) #`(if (eq? v 'p) #,(k binds) #f)]
        [p #`(if (equal? v 'p) #,(k binds) #f)])))

  (define (convert-clause c)
    (syntax-case c (guard)
      [(p (guard g ...) e es ...) #'(p (guard g ...) (begin e es ...))]
      [(p e es ...) #'(p (guard #t) (begin e es ...))]
      [_ (errorf 'match "invalid clause ~s" (syntax->datum c))]))

  (define (eliminate g k ok no)
    (define (false? x) (eq? x #f))
    (define (true? x) (and (not (eq? x #f)) (atom? x)))
    (with-syntax ([(guard . g) g] [k k] [ok ok] [no no])
      (cond
        [(andmap true? (syntax->datum #'g))
         #'(call-with-values (lambda () ok) k)]
        [(ormap false? (syntax->datum #'g)) #'no]
        [else #`(if (and #,@#'g)
                    (call-with-values (lambda () ok) k)
                    no)])))

  (define (parse-clause k v c)
    (with-syntax ([(p g ok) (convert-clause c)])
      (parse-pattern #'p #'v '()
        (lambda (binds)
          (eliminate #'g k #'ok #f)))))

  (define (parse-clauses k v cs)
    (syntax-case cs ()
      [(c . cs)
       (syntax-case (parse-clause k v #'c) (call-with-values)
         [(call-with-values p c) #'((call-with-values p c))]
         [#f (parse-clauses k v #'cs)]
         [c0 (cons #'c0 (parse-clauses k v #'cs))])]
      [() #`((errorf 'match "failed to match ~s" #,v))]))

  (syntax-case x ()
    [(_ e c0 c1 ...)
     (lambda (r)
       (set! lookup r)
       #`(let ([v e])
           (call/cc
             (lambda (k)
               #,@(parse-clauses #'k #'v #'(c0 c1 ...))))))]))


(define (gensymmap fmt xs)
  (define (gensym/format x)
    (datum->syntax #'* (gensym (format fmt x))))
  (if (pair? xs)
      (map gensym/format xs)
      (gensym/format xs)))

(define (formatsym fmt xs)
  (string->symbol (apply format fmt xs)))

(define (symbolic=? x)
  (let ([x (syntax->datum x)])
    (lambda (y) (eq? x (syntax->datum y)))))

(define (unique? xs)
  (or (null? xs)
      (and (not (memq (car xs) (cdr xs)))
           (unique? (cdr xs)))))

(define (check-unique fns p)
  (unless (unique? fns)
    (errorf 'match "duplicated fields found in the record pattern ~a" p)))


;; Uncomment this code to run tests
#;
(let ()

(match '(let-values
          ([(x y z) (values 1 2 3)] [(a b c) (values 'a 'b 'c)])
          (list a b c x y z))
  [(let-values ([(,(symbol? vars) ...) ,exps] ...) ,body ...)
   (printf "~a\n" `(let-values ,vars ,exps ,body))])
;; (let-values ((x y z) (a b c)) ((values 1 2 3) (values 'a 'b 'c))
;;   ((list a b c x y z)))


(match '(x y z 1 2 3 . end)
  [(,(symbol? syms) ... ,rest ... . end)
   (printf "syms: ~a\n" syms)
   (printf "rest: ~a\n" rest)])
;; syms: (x y z)
;; rest: (1 2 3)


(match '(1 1 (1))
  [(,x ,x (,x))
   (printf "x=~a\n" x)])
;; x=1


(match '(1 2 3 (1 2 3))
  [(,x ... (,x ...))
   (printf "x=~a\n" x)])
;; x=(1 2 3)


(match '(1 2 3 1 2 3 (1 2 3))
  [(,x ... ,x ... (,x ...))
   (printf "x=~a\n" x)])
;; x=(1 2 3)


(match '(1 2 3 1 2 3 (1 2 3))
  [(,x ... ,y ... (,z ...))
   (printf "x=~a, y=~a, z=~a\n" x y z)])
;; x=(1 2 3 1 2 3), y=(), z=(1 2 3)


(let ()
  (define-record-type pt (fields x y))
  (match (make-pt 1 2)
    [(pt ,y ,x) (printf "(point ~a, ~a)\n" x y)]))
)

;; Uncomment this code to output the target code
#;
(parameterize ([print-gensym 'pretty/suffix])
  (pretty-print
    (expand
      '(match '(1 2 3 1 2 3 (1 2 3))
         [(,x ... ,x ... (,x ...))
          (printf "x=~a\n" x)]
         [_ (guard #f) #f]
         [_ (guard #t) #t]
         ))))
;; The output is shown below

#;
(let ([v.0 '(1 2 3 1 2 3 (1 2 3))])
  (call/cc
    (lambda (k.1)
      ((letrec ([next.2 (lambda (pair.3 x*.4)
                          (let ([t.5 (if (pair? pair.3)
                                         (let ([vcar.6 (car pair.3)])
                                           (let ([x.7 vcar.6])
                                             (next.2 (cdr pair.3) (cons x.7 x*.4))))
                                         #f)])
                            (if t.5
                                t.5
                                (let ([x*.8 (reverse x*.4)])
                                  (let ([x.9 x*.8])
                                    ((letrec ([next.10 (lambda (pair.11 x*.12)
                                                         (let ([t.13 (if (pair? pair.11)
                                                                         (let ([vcar.14 (car pair.11)])
                                                                           (let ([x.15 vcar.14])
                                                                             (next.10 (cdr pair.11) (cons x.15
                                                                                             x*.12))))
                                                                         #f)])
                                                           (if t.13
                                                               t.13
                                                               (let ([x*.16 (reverse x*.12)])
                                                                 (if (equal? x*.16 x.9)
                                                                     (if (pair? pair.11)
                                                                         (let ([vcar.17 (car pair.11)] [vcdr.18 (cdr pair.11)])
                                                                           ((letrec ([next.19 (lambda (pair.20 x*.21)
                                                                                                (let ([t.22 (if (pair? pair.20)
                                                                                                                (let ([vcar.23 (car pair.20)])
                                                                                                                  (let ([x.24 vcar.23])
                                                                                                                    (next.19 (cdr pair.20) (cons x.24
                                                                                                                                    x*.21))))
                                                                                                                #f)])
                                                                                                  (if t.22
                                                                                                      t.22
                                                                                                      (let ([x*.25 (reverse x*.21)])
                                                                                                        (if (equal? x*.25 x.9)
                                                                                                            (if (null? pair.20)
                                                                                                                (if (null? vcdr.18)
                                                                                                                    (call-with-values
                                                                                                                      (lambda ()
                                                                                                                        (printf "x=~a\n" x.9))
                                                                                                                      k.1)
                                                                                                                    #f)
                                                                                                                #f)
                                                                                                            #f)))))])
                                                                              next.19) vcar.17 '()))
                                                                         #f)
                                                                     #f)))))])
                                       next.10) pair.3 '()))))))])
         next.2) v.0 '())
      (call-with-values (lambda () #t) k.1))))
