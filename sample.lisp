(begin
  (+ 1 (print (+ 2 3)))
  (print (lambda (x) (+ x 1)))
  (let (x 3)
    (begin
      (print x)
      (print (+ x 1))))
  (print x)
  (+ 1 2 3))

