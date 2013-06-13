; IDEA: make a test method

(define atom? (lambda (x) (not (pair? x))))

(atom? 1)
(atom? '(123))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(bacon (123 abc) eggs cheese))
;Value: #f
(lat? '(bacon cheese))
;Value: #t

(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? (car l) a) #t)
     (else (member? a (cdr l))))))

(member? 'a '(b c))
;Value: #f
(member? 'a ('b a c))
;Value: #t

(do
    (display "hello"))

(null? (cdr (cdr '(bacon eggs cheese))))

(car '(bacon eggs))
