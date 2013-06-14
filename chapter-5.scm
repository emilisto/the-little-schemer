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

