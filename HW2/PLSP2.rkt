;John Donnelly jed126

;Dot product cps
(define dotproduct
  (lambda (l1 l2 return)
    (cond
    ((null? l1) (return 0))
    ((null? l2) (return 0))
    ((eq? (length l1) 1) (return (* (car l1) (car l2))))
    (else (dotproduct (cdr l1) (cdr l2) (lambda (v) (return (+(* (car l1) (car l2)) v))))))))
  
;remove subsequence cps
(define removesubsequence
  (lambda (subl l return)
    (cond
      ((null? l) (return'()))
      ((null? subl) (return l))
      ((eq? (car subl) (car l)) (removesubsequence(cdr subl) (cdr l) return))
      (else (removesubsequence subl (cdr l) (lambda (v) (return (cons (car l) v))))))))

;squareroot cps
(define squareroot-cps
  (lambda (value iter return)
    (cond
      ((zero? iter) (return value))
      (else (squareroot-cps value (- iter 1) (lambda (v) (return (- v (/ (- (* v v) value) (* 2 v))))))))))

;repleaceall* cps
(define replaceall*
  (lambda (a x l return)
    (cond
      ((null? l) (return '()))
      ((list? (car l)) (replaceall* a x (car l) (lambda (v1) (replaceall* a x (cdr l) (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car l) a) (replaceall* a x (cdr l) (lambda (v) (return (cons x v)))))
      (else (replaceall* a x (cdr l) (lambda (v) (return (cons (car l) v))))))))
      
;reverse* cps

(define reverse*
  (lambda (l return)
    (cond
      ((null? l) (return '()))
      ((list? (car l)) (reverse* (cdr l) (lambda (v) (reverse* (car l) (lambda (v2) (my-append-cps v (cons v2 '( )) return))))))
      (else (reverse* (cdr l) (lambda (v) (return (my-append-cps v (cons (car l) '( )) (lambda (v) v)))))))))


(define my-append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (my-append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

;vectormult cps
(define vectormult
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return 0))
      ((null? l2) (return 0))
      ((null? (car l2)) (return null))
      (else (dotproduct l1 (vectormaker 1 l2 (lambda (v) v)) (lambda (v1) (vectormult l1 (vectorcutter l2 (lambda (v3) v3)) (lambda (v2) (return (cons v1 v2))))))))))

;this will go through a matrix and create a vector out of columns, useful for creating a format for dot product
(define vectormaker
  (lambda (n l return)
    (cond
      ((null? l)(return l))
      ((null? (car l)) (return l))
      ((eq? (len l (lambda (v) v)) 0) (return'()))
      (else (vectormaker n (cdr l) (lambda (v1) (return (cons (car (car l)) v1 ))))))))

;This will slash off the first column of a matrix
(define vectorcutter
  (lambda (l return)
    (cond
      ((null? l) (return l))
      ((list? (car l)) (vectorcutter (car l) (lambda (v1) (vectorcutter (cdr l) (lambda (v2) (return (cons v1 v2)))))))
      (else (return (cdr l))))))

;find the nth item in a list
(define len
  (lambda (l return)
    (if (null? l)
        (return 0)
        (len (cdr l) (lambda (v) (return (+ 1 v)))))))

;matrixmultuply cps
(define matrixmultiply
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return null))
      ((null? l2) (return null))
      (else (vectormult (car l1) l2 (lambda (v1) (matrixmultiply (cdr l1) l2 (lambda (v2) (return (cons v1 v2))))))))))

;removesubsequence cps
(define removesubsequence*
  (lambda (subl l return)
    (cond
      ((null? l) (return'()))
      ((null? subl) (return l))
      ((list? (car l)) (removesubsequence* subl (car l) (lambda (v1) (removesubsequence* (cons (car subl) '()) (cdr l) (lambda (v2) (return(cons v1 v2 )))))))
      ((eq? (car subl) (car l)) (removesubsequence*(cdr subl) (cdr l) return))
      (else (removesubsequence* subl (cdr l) (lambda (v) (return (cons (car l) v))))))))
;needed a way to check if the the sublist found a match in an inner list to make work properly
;suffix cps no helper functions
(define suffix*
  (lambda (a l return)
    (letrec ([reverse* (lambda (l return)
                         (cond
                           ((null? l) (return '()))
                           ((list? (car l)) (reverse* (cdr l) (lambda (v) (reverse* (car l) (lambda (v2) (my-append-cps v (cons v2 '( )) return))))))
                           (else (reverse* (cdr l) (lambda (v) (return (my-append-cps v (cons (car l) '( )) (lambda (v) v))))))))]
             
             [finder (lambda (a l1 return)
                       (cond
                         ((null? l1) (return '()))
                         ((eq? a (car l1)) (return '()))
                         (else (finder a (cdr l1) (lambda (v5) (return (cons (car l1) v5)))))))])
      (reverse* (finder a (reverse* l (lambda (v1) v1)) (lambda (v2) v2)) (lambda (v6) v6)))))

;suffic cps call/cc
(define suffix2*
  (lambda (a l return)
    (call/cc
     (lambda (break)
    (letrec ([reverse* (lambda (l return)
                         (cond
                           ((null? l) (return '()))
                           ((list? (car l)) (reverse* (cdr l) (lambda (v) (reverse* (car l) (lambda (v2) (my-append-cps v (cons v2 '( )) return))))))
                           (else (reverse* (cdr l) (lambda (v) (return (my-append-cps v (cons (car l) '( )) (lambda (v) v))))))))]
             [finder (lambda (a l1 return)
                        (cond
                         ((null? l1) (break '()))
                         ((eq? a (car l1)) (return '()))
                         (else (finder a (cdr l1) (lambda (v5) (return (cons (car l1) v5)))))))])
      (reverse* (finder a (reverse* l (lambda (v1) v1)) (lambda (v2) v2)) (lambda (v6) v6)))))))
