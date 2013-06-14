(load "chapter-1-3.scm")

;; rember*: deep-removal of atom from a list, i.e. by traversing all sublists
(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ; If first element is a, remove and recurse on cdr
       ((eq? (car l) a) (rember* a (cdr l)))
       ; Else, go on
       (else (cons (car l) (rember* a (cdr l))))
       ))
     ; Run on the sub list
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
              (occur* a (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

;; INTERMISSION
;; Note how all these three follow the same basic pattern:

(define fn
  (lambda (a l)
    (cond
     ((null l) 'end-of-list-value)
     ((atom? (car l))
      (cond
       ('criteria-met)
       (else (fn a (cdr l)))))
     (else ('join-operation
            (fn a (car l))
            (fn a (cdr l)))))))

; This seems to be the skeleton for all recursive list matches - could
; these be made a macro?

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

; Does leftmost deserve a star? No, because it only recurs on the car,
; and nost the 
