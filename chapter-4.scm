;; Lists whose atoms are all numbers are called tup's

;; Lotsa repetitive stuff where they instead show how tups can be
;; used to store numbers, and how we can define the arithmetic
;; operations on these lists as recursive function, starting only from
;; sub1 and add1, which adds and subtracts from numbers respectively.
;;
;; Point being that lists are versatile and recursive operations are
;; powerul.

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else
      (+ 1 (length (cdr l)))))))

(define pick
  (lambda (n l)
    (cond
     ((zero? (- n 1)) (car l))
     (else (pick (- n 1) (cdr l))))))

; They go into the concept of "primitive functions", e.g. one could
; not implement the function number? Same goes for:
; add1, sub1, zero? car, cdr, cons, null? eq? atom? (exception for
; this one, I do in fact implement it using _another_ primitive
; function pair, which is (not (atom? v))
