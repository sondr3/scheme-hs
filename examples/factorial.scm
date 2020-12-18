(define (factorial n)
  (if (= 0 n)
    1
    (* n (factorial (- 1 n)))))