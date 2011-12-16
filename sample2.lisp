;(begin
;  (define gen
;    (lambda () (begin
;                 (define i 0)
;                 (lambda () i))))
;  (print gen)
;  (define f (gen))
;  (print f)
;  (print (f)))
(begin
  (define sum (lambda (n) (if (== n 0) 0 (+ (sum (- n 1)) n))))
  (print (sum 10)))
(begin
  (define a 1)
  (comment
  (define f (lambda ()
              (begin
                (print a)
                (define a 999)
                (print a)))))
  (f)
  (define a 10)
  (f))
