; SYNTAX TEST "Scheme.sublime-syntax"


;###########
; SYNTAXES #
;###########

(lambda (x . y) (cons x y))
;^^^^^^ storage.type.function.inline.scheme keyword.declaration.function.inline.scheme
;        ^ meta.lambda.scheme variable.parameter.scheme
;          ^ meta.lambda.scheme keyword.control.scheme

(define name value)
;^^^^^^ keyword.declaration.scheme
;       ^^^^ entity.name.function.scheme
;            ^^^^^ entity.other.scheme

(set! name value)
;^^^^ keyword.assignment.scheme
;     ^^^^ entity.name.function.scheme
;          ^^^^^ entity.other.scheme

(let ([x 0] [y 1]) (+ x y))
;^^^ meta.let.scheme keyword.control.scheme
;      ^ meta.let.scheme variable.parameter.scheme
;                   ^ meta.let.scheme meta.function-call.scheme support.function.scheme

(let f ([x 13] [y 130])
;    ^ meta.named-let.scheme entity.name.function.scheme
  (if (< x y)
;  ^^ meta.named-let.scheme keyword.control.scheme
      (f (+ x 2) (+ y 1))
      (list x y)))

(case x
  [(lambda let) 'keywords]
;   ^^^^^^ meta.case.scheme constant.symbol.literal.scheme
  [else 'other])
;^^^^^^^^^^^^^^ meta.case.scheme

(syntax-case x (quote unquote)
;^^^^^^^^^^^ meta.syntax-case.scheme keyword.control.syntax.scheme
;            ^ entity.other.scheme
;               ^^^^^ meta.syntax-case.scheme constant.symbol.literal.scheme
  [,x #`(let ([x v]) ...)]
;     ^^ meta.syntax-case.scheme keyword.quasisyntax.scheme
;                    ^^^ meta.syntax-case.scheme meta.let.scheme keyword.aux.scheme
  [ x #'(if (equal? v 'x) ... ...)])
;     ^^ meta.syntax-case.scheme keyword.syntax.scheme


;###########
; COMMENTS #
;###########

 ; comment
;^ comment.line.scheme punctuation.definition.comment.scheme
;^^^^^^^^^^ comment.line.scheme

 #|
;^^ comment.block.scheme punctuation.definition.comment.begin.scheme
  This is a multi-line comment.
; ^^^^^ comment.block
  #|
    They can be nested!
  |#
; ^^ comment.block.scheme comment.block.scheme punctuation.definition.comment.end.scheme
|#

(
; comments inside lists
; ^^^^^^^^^^^^^^^^^^^^^^ comment.line.scheme

  #| #| block comments |# |#
;    ^^ comment.block.scheme comment.block.scheme punctuation.definition.comment.begin.scheme
)


(#| stray comment inside list |#)
;^^ comment.block.scheme punctuation.definition.comment.begin.scheme

( #| invalid paren |#
 ([)])
;  ^ invalid.illegal.close-paren.scheme
 (])
; ^ invalid.illegal.close-bracket.scheme
 ))))
; ^^^ invalid.illegal.close-paren.scheme


;###########;##########
; EXPRESSION COMMENTS #
;###########;##########

 #; #'() 1
;^^ meta.comment.expression.scheme punctuation.definition.comment.expression.scheme
;   ^^^^ meta.comment.expression.scheme meta.syntax.scheme
;        ^ - meta.comment

 #; '#'() 1
;^^ meta.comment.expression.scheme punctuation.definition.comment.expression.scheme
;   ^^^^^ meta.comment.expression.scheme meta.quote.scheme
;        ^^ - meta.comment

 #; #'x 1
;   ^^^ meta.comment.expression.scheme meta.syntax.scheme
;       ^ - meta.comment

 #; #'1 1
;   ^^^ meta.comment.expression.scheme meta.syntax.scheme
;       ^ - meta.comment

 #;
;^^ meta.comment.expression.scheme punctuation.definition.comment.expression.scheme
 1 #; #;
;^ meta.comment.expression.scheme constant.numeric.decimal.scheme
;  ^^ punctuation.definition.comment.expression.scheme
;     ^^ punctuation.definition.comment.expression.scheme
 2 3 4
;^^^ meta.comment.expression.scheme
;    ^ - meta.comment

 #;#;#;#;1 2 3 4 5
;^^^^^^^^ punctuation.definition.comment.expression.scheme
;^^^^^^^^^^^^^^^ meta.comment.expression.scheme
;                ^ - meta.comment

 #;#;#;#; ; 1 2 3 4 5
;^^^^^^^^ punctuation.definition.comment.expression.scheme
;         ^^^^^^^^^^^ comment.line.scheme
 6 7 8 9 10
;^^^^^^^ meta.comment.expression.scheme
;        ^^ - meta.comment

 #; x #; #; (+ 1 3) 98 99
;   ^ meta.comment.expression.scheme entity.other.scheme
;    ^ - meta.comment
;     ^^^^^^^^^^^^^^^^ meta.comment.expression.scheme
;     ^^ punctuation.definition.comment.expression.scheme
;        ^^ punctuation.definition.comment.expression.scheme
;                      ^^ - meta.comment

 #;()()
;^^^^ meta.comment.expression.scheme
;    ^^ - meta.comment

 #;
;^^ meta.comment.expression.scheme punctuation.definition.comment.expression.scheme
 #| balabala |#  1 2
;^^ comment.block.scheme punctuation.definition.comment.begin.scheme
;   ^^^^^^^^^^^ meta.comment.expression.scheme comment.block.scheme
;                ^ meta.comment.expression.scheme constant.numeric.decimal.scheme
;                  ^ - meta.comment

 #;"x" y
;^^^^^ meta.comment.expression.scheme
;      ^ - meta.comment

 #;"\x\n\"" y
;^^^^^^^^^^ meta.comment.expression.scheme
;  ^^^^^^^^ meta.comment.expression.scheme string.quoted.double.scheme
;           ^ - meta.comment

 #;("Hello World)))))") 1
;^^^^^^^^^^^^^^^^^^^^^^ meta.comment.expression.scheme
;   ^^^^^^^^^^^^^^^^^^ string.quoted.double.scheme
;                       ^ - meta.comment

 #;#{x y} 1
;^^^^^^^^ meta.comment.expression.scheme
;         ^ - meta.comment

 #;(define-syntax alias ; already built-in
     (lambda (x) syntax-error x "misplaced aux keyword"))
;     ^^^^^^ meta.comment.expression.scheme storage.type.function.inline.scheme
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.comment.expression.scheme

 #;([)])
;  ^^^^^ meta.comment.expression.scheme
;    ^ meta.comment.expression.scheme invalid.illegal.close-paren.scheme


;##########
; STRINGS #
;##########

(format "~s" "Hello, world!")
;        ^^ string.quoted.double.scheme constant.other.placeholder.scheme

("string\n")
;^ string.quoted.double.scheme punctuation.definition.string.begin.scheme
;^^^^^^^^^^ string.quoted.double.scheme
;       ^^ string.quoted.double.scheme constant.character.escape.scheme
;         ^ string.quoted.double.scheme punctuation.definition.string.end.scheme

("multi-line string
    ends here"
;            ^ string.quoted.double.scheme punctuation.definition.string.end.scheme
)

;##########
; NUMBERS #
;##########

 10
;^^ constant.numeric.decimal.scheme

 1.7
;^^^ constant.numeric.decimal.scheme

 1.5e+4
;^^^^^^ constant.numeric.decimal.scheme

 #xF0A
;^^ constant.numeric.hex.scheme storage.modifier.numeric.scheme
;^^^^^ constant.numeric.hex.scheme

 #e#b0
;^^^^ constant.numeric.binary.scheme storage.modifier.numeric.scheme
;    ^ constant.numeric.binary.scheme


;#############
; CHARACTERS #
;#############

 #\ x
;^^ storage.modifier.character.scheme
;   ^ invalid.illegal.delimiter.scheme

 #\x
;  ^ constant.character.scheme

 #\newline
;  ^^^^^^^ constant.character.scheme

 #\xaf
;^^^ storage.modifier.character.scheme
;   ^^ constant.character.numeric.hex.scheme

 #\000
;^^^ storage.modifier.character.scheme
;   ^^ constant.character.numeric.octal.scheme

 #\377
;^^ storage.modifier.character.scheme
;  ^^^ constant.character.numeric.decimal.scheme


;################
; TYPE STORAGES #
;################

 #0=()
;^^^ storage.modifier.pair.scheme

 #0()
;^^ storage.modifier.vector.scheme
; ^ storage.modifier.vector.scheme constant.numeric.scheme
;  ^^ punctuation.paren

 #[]
;^ storage.modifier.record-name.scheme

 #{x y}
;^^ storage.modifier.gensym.begin.scheme
;  ^ string.gensym.pretty-name.scheme
;    ^ string.gensym.unique-name.scheme
;     ^ storage.modifier.gensym.end.scheme

 #&()
;^^ storage.modifier.boxes.scheme


;#####################
; LANGUAGE CONSTANTS #
;#####################

 #fx
;^^ constant.boolean.scheme
;  ^ invalid.illegal.delimiter.scheme

 #t
;^^ constant.boolean.scheme

'()
;^^ constant.language.scheme

'www
;^^^ symbol.quoted.scheme

'null?
;^^^^^ symbol.quoted.scheme

'|\|
;^^^ symbol.quoted.scheme

'1000
;^^^^ constant.numeric.decimal.scheme


;####################
; SUPPORT FUNCTIONS #
;####################

(1+ a b)
;^^ meta.function-call.scheme support.function.scheme

(+ a b)
;^ meta.function-call.scheme support.function.scheme

(- a b)
;^ meta.function-call.scheme support.function.scheme

(mod a b)
;^^^ meta.function-call.scheme support.function.scheme

 list
;^^^^ support.function.scheme


;##############
; APPLICATION #
;##############

(f x y)
;^ meta.function-call.scheme variable.function.scheme

'(100 100 100)
; ^^^ constant.numeric.decimal.scheme

'(100 . 100)
; ^^^ constant.numeric.decimal.scheme
;     ^ keyword.aux.scheme
;       ^^^ constant.numeric.decimal.scheme

'(100 x 100)
; ^^^ constant.numeric.decimal.scheme
;     ^ entity.other.scheme
;       ^^^ constant.numeric.decimal.scheme

'(1+1 100 100)
; ^^^ meta.function-call.scheme variable.function.scheme

'( 1+1 100 100)
;  ^^^ meta.function-call.scheme variable.function.scheme

 (define |x| 0)
;        ^^^ entity.name.function.scheme

(|x| 1 2 3)
;^^^ meta.function-call.scheme variable.function.scheme
