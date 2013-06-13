; IDEA: make a test method

;; atom?: is it not a list?
(define atom? (lambda (x) (not (pair? x))))

(atom? 1)
(atom? '(123))

;; lat?: are there only atoms in a list?
(define lat?
  (lambda (l)
    (cond

; 1ST COMMANDMENT: Always ask null? as the first question in
; expressing any function.
     ((null? l) #t)
     
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(bacon (123 abc) eggs cheese))
;Value: #f
(lat? '(bacon cheese))
;Value: #t


;; member?: is a given atom a member of a list?
(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     (else (or (eq? (car l) a)
               (member? a (cdr l)))))))

(member? 'a '(b c))
;Value: #f
(member? 'a '(b a c))
;Value: #t


;; rember: return the list, but with the member atom a removed, if in the list.
(define rember
  (lambda (a l)
    (cond
     ((null? l) '())
     ((eq? (car l) a) (cdr l))
     (else

     ; 2ND COMMANDMENT: Use cons to build lists.
     ; 3RD COMMANDMENT: When building a list, describe the first
     ; typical element, and then cons it onto the natural recursion.
      (cons (car l) (rember a (cdr l)))))))

(rember 'fucking '(seven dirty words you cant fucking say on radio or television))


;; map
(define map
  (lambda (fn l)
    (cond
     ((null? l) '())
     ; concatenate
     (else (cons
            ; the list of the transformed first element
            (fn (car l))
            ; with the mapped remaining elements, ad in-nullinum.
            (map fn (cdr l)))))))

(map atom? '(imone (imnot) and us ))
; (#t #f #t #t)


;; firsts

; A feeling tells me this is where one could use some sort of
; define-macro, to essentially left-curry the map function.
(define firsts
  (lambda (l) (map (lambda (sublist) (car sublist)) l)))

(firsts '((a b c) (b a c) (c a b)))
; (a b c)



;; insertR
(define insertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? (car l) old)
             (cons old (cons new (cdr l))))
            (else (cons
                   (car l)
                   (insertR new old (cdr l)))))))))

(insertR '"wind at" 'breaking '(im breaking dawn))
; (im breaking "wind at" dawn)

;; insertL
(define insertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? (car l) old)
             (cons new l))
            (else (cons
                   (car l)
                   (insertL new old (cdr l)))))))))

(insertL 'hating 'breaking '(i am breaking dawn))
; (im hatin breaking dawn)

;; 4TH COMMANDMENT: Always change an argument while recurring.


;; Gambling and rambling
(do
    (display "hello"))

(null? (cdr (cdr '(bacon eggs cheese))))

(car '(bacon eggs))
