;; It is tempting to define zero and one like this:
;;
;; (define zero '())
;; (define one '(()))
;;
;; However, this doesn't make much sense - since their definitions are
;; directly connected to the definitions of zero?, add1 and sub1.
;; Hence their definitions are contained in the following three
;; pseudo-primitive functions:

(define zero?
 (lambda (n)
   (null? n)))

(define add1
  (lambda (n)
    (cons '() n)))

(define sub1
  (lambda (n)
    (cdr n)))

;; We can now define some common arithmetic operations from the
;; previous three primitive operations:
(define +
  (lambda (a b)
    (cond
     ((zero? a) b)
     (else (+ (sub1 a) (add1 b))))))

(define -
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (- (sub1 a) (sub1 b))))))

(define *
  (lambda (a b)
    (cond
     ;; Hmm, we explicitly define zero here.
     ((zero? b) '())
     (else (+ a (* a (sub1 b)))))))

(define pow
  (lambda (a b)
    (cond
     ;; And here we specify one, perhaps these ought to be atoms anyway?
     ((zero? b) '(()))
     (else (* a (pow a (sub1 b)))))))
