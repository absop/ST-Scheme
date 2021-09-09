(begin
  (pretty-format 'when '(_ e 5 ...))

  ; (pretty-file "{infile}" "{outfile}")
  #| single level block comment |#
  #| #| double level block comment |# |#
  #| nothing |#
  `(,'expr)
  (error #f "none")
  (define 你好 'hello)
  (printf "~s\n" "done!")
  (printf "~s\n" "hello \\\
          ")
  (printf "~s\n" "hello \\
                  world"))