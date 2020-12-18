(define Y                 
  (lambda (f)             
    ((lambda (g) (g g))   
     (lambda (g)       
       (f  (lambda a (apply (g g) a))))))) 