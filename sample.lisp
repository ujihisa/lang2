(begin
  (define hello
    (lambda (ignore) (print 999)))
  (print print)
  (+ 1 (print (+ 2 3)))
  (print (lambda (x) (+ x 1)))
  ((lambda (x) (print x)) 123)
  (let (x 3)
    (begin
      (print x)
      (print (+ x 1))))
  (print x)
  (hello 1)
  (print
    (if 1 (+ 2 3) (+ 4 5)))
  (comment (define sum
             (lambda (n)
               (if (== n 0) 1 (+ n (sum (- n 1))))))
           (print (sum 10))))
