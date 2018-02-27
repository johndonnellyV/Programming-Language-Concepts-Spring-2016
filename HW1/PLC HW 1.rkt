;John Donnelly

;1. inorder which takes a list of numbers and returns true of they are in non-decreasing order
(define inorder?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((null? (cdr l)) #t)
      ((> (car l) (car (cdr l))) #f)
      ((< (car l) (car (cdr l))) (inorder?(cdr l))))))
;2. dotproduct takes a two vectors (lists of numbers) and computes the dot product of the vectors. If one list is longer than the other, you can ignore the extra numbers of the longer list. 
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      ((eq? (len l1) 1)(* (car l1) (car l2)))
      (else (+(* (car l1) (car l2)) (dotproduct(cdr l1) (cdr l2)))))))
;simple length method to help
(define len
  (lambda (lis)
    (if (null? lis)
        0
        (+ 1(len(cdr lis))))))
;3. squareroot takes two numbers, a value and an iteration. The iteration will be an integer greater than or equal to 0. The method will compute the squareroot of the value using iteration rounds of Newton's method, starting with an initial value equal to the input value.
;Newton's method is new = old - ((old * old) - value) / (2 * old)
(define squareroot
  (lambda (l i)
    (cond
      ((eq? l 0) 0)
      (else(newtons* l i l)))))
;method doing the actual method calculations
(define newtons*
  (lambda (old count value)
    (cond
      ((eq? count 0) old)
      (else(newtons*(- old (/ (- (* old old) value ) (* 2 old))) (- count 1) value)))))

      

;4. removesubsequence takes two lists of atoms. The first list is a subsequence of the second list. The method should return the second list with the first occurence of the subsequence removed. So,
;if the first list is '(a b c), the first a if the second list is removed,the first b that appears after the removed a is removed, and the first c that appears after the removed b is removed.
(define removesubsequence
  (lambda (subl l)
    (cond
      ((null? subl) l)
      ((null? l) '())
      ((eq? (car subl) (car l)) (removesubsequence(cdr subl) (cdr l)))
      (else (cons (car l) (removesubsequence subl (cdr l)))))))

;5. reverse* takes a nested list and reverses the contents of the list and all nested lists
;myappend '(a b) '( 1 2 3)  returns (a b 1 2 3)
(define myappend
  (lambda (a b)
    (cond
      ((null? a) b)
      (else(cons (car a) (myappend(cdr a) b))))))


;my reverse '(1 2 3) returns '(3 2 1)
(define reverse*
  (lambda (l)
    (cond
      ((null? l)'())
      ((list? (car l)) (myappend (reverse* (cdr l)) (cons (reverse*(car l)) '())))
      (else (myappend (reverse* (cdr l)) (cons (car l) '()))))))

;6. first* takes a list of lists and returns the first atom that appears in the list, regardless of how nested it is
(define first*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (first*(car l)))
      (else (car l)))))

;7. last* takes a list of lists and returns the last atom that appears in the list, regardless of how nested it is
(define last*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and(list? (car l)) (not (null? (cdr l)))) (last* (cdr l)))
      ((and(list? (car l)) (null? (cdr l))) (last*(car (car l))))
      ((null? (cdr l)) (car l))
      (else (last*(cdr l))))))

;8. numorder*? takes a possibly nested list of numbers, and returns #t if the values of the entries in the list are in non-decreasing order. The value of a number is the number. The value of a list is the sum of the values in that list.
;(define numorder*?
  ;(lambda (l)
    ;(cond
      ;((null? l) #t)
      ;((list? (car l)) (numorder*?(cons (sumnumbers* (car l)) (cdr l))))
      ;((and (not(null? (cdr l)))
      ;((list? (cadr l)) (numorder*?(cons (car l) (cons(sumnumbers*(cadr l)) '()))) (numorder*?(cdr(cadr l)) ))
      ;((<= (car l) (car (cdr l))) (numorder*?(cdr l)))
      ;(else #f))))
(define numorder*?
  (lambda (l)
      (cond
        ((null? l) #t)
        ((eq? (len l) 1) #t)
        ((list? (car l)) (numorder*?(cons(sumnumbers* (car l)) (cdr l))))
        ((and(not(null? (cdr (cdr l)))) 
              (list? (cadr l))) (and(numorder*? (cadr l)) (numorder*?(cons (car l) (cons(sumnumbers*(cadr l)) (cdr (cdr l)))))))
        ((and(null? (cdr(cdr l)))
             (list? (cadr l))) (and(numorder*?(cadr l)) (numorder*?(cons (car l) (cons(sumnumbers*(cadr l)) '())))))
        ((>= (car (cdr l)) (car l)) (numorder*? (cdr l)))
        (else #f))))
     
;This takes a list and sums all the numbers inside it returning just that number
(define sumnumbers*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? l) l)
      ((number? (car l)) (+ (car l) (sumnumbers* (cdr l))))
      ((list? (car l)) (+ (sumnumbers* (car l)) (sumnumbers* (cdr l))))
      (else (sumnumbers* (cdr l))))))

;9. vectormult takes a row vector (a list of numbers) and matrix (a list of lists of numbers) and multiplies the vector times the matrix. The result is a vector where the ith element of the result is the dotproduct of the input vector and the ith column of the matrix.
;You can assume that the length of the vector matches the number of rows of the matrix.
(define vectormult
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      ((null? (car l2)) null)
      (else(cons(dotproduct l1 (vectormaker 1 l2)) (vectormult l1 (vectorcutter l2)))))))
;this will go through a matrix and create a vector out of columns, useful for creating a format for dot product
(define vectormaker
  (lambda (n l)
    (cond
      ((null? l)l)
      ((null? (car l)) l)
      ((eq? (len l) 0) '())
      (else(cons (itemn(car l) n) (vectormaker n (cdr l)))))))
;This will slash off the first column of a matrix
(define vectorcutter
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (cons(vectorcutter(car l)) (vectorcutter(cdr l))))
      (else (cdr l)))))
;find the nth item in a list
 (define itemn
   (lambda (l n) 
      (cond
        ((eq? n 0) '())
        ((eq? n 1) (car l))
        (else (itemn (cdr l) (- n 1))))))

;10. matrixmultiply takes two matrices (a list of lists of numbers) and multiplies them. You can assume the number of columns of the first matrix is equal to the number of rows of the second matrix.
;in the same sublist

(define matrixmultiply
  (lambda (l1 l2)
    (cond
      ((null? l1) null)
      ((null? l2) null)
      (else (cons (vectormult (car l1) l2) (matrixmultiply (cdr l1) l2))))))