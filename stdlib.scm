(define (not x)
  (if x #f #t))

(define (null? obj)
  (if (eqv? obj '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip fun)
  (lambda (arg1 arg2) (fun arg2 arg1)))

(define (curry fun arg1)
  (lambda (arg)
    (apply fun (cons arg1 (list arg)))))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num)
  (= (modulo num 2) 1))
(define (even? num)
  (= (modulo num 2) 0))

(define (foldr fun end lst)
  (if (null? lst)
      end
      (fun (car lst) (foldr fun end (cdr lst)))))

(define (foldl fun acc lst)
  (if (null? lst)
      acc
      (foldl fun (fun acc (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold fun init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold fun (fun init) pred))))

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 0 lst))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new)) first  rest))

(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new)) first rest))

(define (length lst)
  (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst)
  (fold (flip cons) '() lst))

(define (mem-helper pred op)
  (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(define (memq obj lst)
  (fold (mem-helper (curry eq? obj) id) #f lst))

(define (memv obj lst)
  (fold (mem-helper (curry eqv? obj) id) #f lst))

(define (member obj lst)
  (fold (mem-helper (curry equal? obj) id) #f lst))

(define (assq obj alist)
  (fold (mem-helper (curry eq? obj) car) #f alist))

(define (assv obj alist)
  (fold (mem-helper (curry eqv? obj) car) #f alist))

(define (assoc obj alist)
  (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map fun lst)
  (foldr (lambda (x y) (cons (f x) y)) '() lst))

(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (boolean? x)
  (or (eq? x #t) (eq? x #f)))
