;;
;; Upon reading The Little Schemer, I was inspired to start defining
;; the basic arithmetic operations, a bit like they do in the book.
;;
;; The cool idea is that the _representation_ of numbers is arbitrary
;; to the operation of the program - it only matters for input, output
;; and efficiency of computation. If one can pin down how numbers are
;; represented in as little code as possible - everything else can be
;; define regardless of the representation we choose.
;;
;; I played around quiet a bit with the basic most basic primitives
;; from which the basic arithmetic operations can then be derived.
;;
;; At first, I didn't define zero and one. However - this meant I had
;; to write them out explicitly in many places, which feels wrong. I
;; ought to read up on some number theory - but after consideration it
;; made sense to define the following as our primitive operations and
;; symbols:
;;
;; zero, one, add1 and sub1
;;

;;
;; First-level primitives

(define zero '())
(define zero?
 (lambda (n)
   (eq? n zero)))

(define add1
  (lambda (n)
    ;; Not happy with this one - we define the same thing twice
    (cons '() n)))
(define sub1
  (lambda (n)
    (cdr n)))

(define one (add1 zero))

;; We now proceed to define some common arithmetic operations, using
;; _only_ the previously defined operations, and logic (i.e. the cond
;; form)

;;
;; Second-level primitives

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
     ((zero? b) zero)
     (else (+ a (* a (sub1 b)))))))

(define pow
  (lambda (a b)
    (cond
     ;; This denotes the agreed-upon convention that any number raised
     ;; to 0 is 1.
     ((zero? b) one)
     (else (* a (pow a (sub1 b)))))))


;;
;; Functions for creating numbers from a decimal-digit representation.

;; I'd like to define this by giving a sequence of atoms (numbers),
;; which defines their order, and then have the base deduced from the
;; length of this list, and each digits corresponding number defined
;; by its position in the list. This would allow for trivial
;; implementation of all sorts of number representations.
(define from-digit
  (lambda (digit)
    (cond
     ((eq? digit 0) zero)
     ((eq? digit 1) (add1 (from-digit 0)))
     ((eq? digit 2) (add1 (from-digit 1)))
     ((eq? digit 3) (add1 (from-digit 2)))
     ((eq? digit 4) (add1 (from-digit 3)))
     ((eq? digit 5) (add1 (from-digit 4)))
     ((eq? digit 6) (add1 (from-digit 5)))
     ((eq? digit 7) (add1 (from-digit 6)))
     ((eq? digit 8) (add1 (from-digit 7)))
     ((eq? digit 9) (add1 (from-digit 8))))))

(define decimal-base (add1 (from-digit 9)))

(define from-decimal
  (lambda (digits)
    (cond
     ((null? digits) zero)
     (else
      (+ (* (from-digit (car digits))
            (pow decimal-base
                 (sub1 (from-digit (length digits)))))
         (from-decimal (cdr digits)))))))

;; to-decimal requires division and moduli operation, leaving for now.
