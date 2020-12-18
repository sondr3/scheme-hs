(define (even l)
  (if (null? l)
      '()
      (if (null? (cdr l))
          '()
          (cons (car (cdr l)) (even (cdr (cdr l)))))))

(define (odd l)
  (if (null? l)
      '()
      (if (null? (cdr l))
          (list (car l))
          (cons (car l) (odd (cdr (cdr l)))))))

(define (merge left right)
  (cond ((null? left) right)
        ((null? right) left)
        ((> (car left) (car right))
         (cons (car right) (merge left (cdr right))))
        (else (cons (car left) (merge (cdr left) right)))))

(define (mergesort l)
  (if (null? l)
      l
      (if (null? (cdr l))
          l
          (merge (mergesort (odd l))
                 (mergesort (even l))))))
