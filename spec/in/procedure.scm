(define (f x)
  (+ x 42))
  
(define (g p x)
  (p x))
  
(g f 23)