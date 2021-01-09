(begin
  (pretty-format 'when '(_ e 5 ...))

  (pretty-file "{infile}" "{outfile}")
  (error #f "none")
  (printf "done!")
  (printf "hello \\\
          ")
  (printf "hello \\
          world")
  )
