;You may test the code below by uncommenting the tests at the
;bottom of the file.

#lang R5RS
#(require racket/trace)

;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.

(define source (with-input-from-file "units.dat" read-file))

;u and v are numbers
;U = u U-normalized
;V = v V-normalized
; U = (u/v) V

;knowing this it seems reasonable that if you have two normalized lists, you
;only need to CONVERT the constant once.  You can then take V and create a
;third list Z.  Then Z would be the final output such that,
;(cons (updated constant) (cons (updated variable) (rest of new list)))

;(define test (list 'c 'd 'e))
;(display (cons 'a (cons 'b test)))

(define to '(27.5 (dyn -1)(furlong 1)(fortnight -1)))
(define from '((mi 1)(hr -1)))

;gets the next variable in the list
;it checks to make sure it is not a constant
(define (get-next-var li)
  (cond ((not (pair? (car li))) (cadr li))
        (else (car li))))

;returns the type of variable
(define (var-name var)
  (car var))

;returns the exponent value of a variable
(define (var-exponent var)
  (cadr var))

;returns the normalized constant and
;variables
(define (get-normalized-value val)
  (cadr (assoc val source)))

;returns a number unattached to a list
(define (get-normalized-constant val)
  (car val))

;returns a list without the constant
(define (remove-constant li)
  (cdr li))

;returns the constant of a list
(define (get-constant li)
  (car li))

;returns a list of variables
(define (get-normalized-vars val)
  (define (iter val2)
    (cond ((null? val2) '())
          (else (cons (car val2)
                      (iter (cdr val2))))))
  (iter (cdr val)))

;combines a list of variables with an
;existing list
(define (add-normalized-vars new old)
  (append new old))

;for normalizing constants altered by a
;negative exponent
(define (update-const-norm-neg u v exp)
  (cond ((= 0 exp) u)
        (else
         (update-const-norm-neg (/ u v) v (- exp 1)))))

;for normalizing constants altered by a
;positive exponent
(define (update-const-norm-pos u v exp)
  (cond ((= 0 exp) u)
        (else
         (update-const-norm-pos (* u v) v (- exp 1)))))

;driver for the two constant normalizer
(define (update-const-norm u v exp)
  (cond ((negative? exp) (update-const-norm-neg u v (abs exp)))
        (else
         (update-const-norm-pos u v (abs exp)))))

;this assumes that the number of vars 
;could conceivably be a multiple of a normalized
;exponent.  for example, if you convert from ft^3
;to m^3, you have to deal with the fact that the
;table returns (m 1), even though the constant has
;been define based on (m 3).
(define (mod-exp new-var old-exp)
  (cond ((null? new-var) '())
        (else
         (cons (cons (var-name (car new-var))
               (cons (* old-exp
                        (var-exponent (car new-var)))
                     '()))
               (mod-exp (cdr new-var) old-exp)))))

;converts a list to a normalized list
;without the contants
(define (normalize-vars li)
  (define (normalizer new old)
    (cond ((null? old) '())
          ((assoc (var-name (get-next-var old))
                  source);returns true if value exists
           (append (mod-exp
                    (add-normalized-vars
                     (get-normalized-vars
                      (get-normalized-value
                       (var-name
                        (get-next-var old))))
                     new)
                    (var-exponent
                     (get-next-var old)))
                 (normalizer
                  '()
                  (cdr old))))
          (else (cons (get-next-var old)
                      (normalizer
                       '()
                       (cdr old))))))
  (normalizer '() (cdr li)))

;this updates the constant whenever a varaible needs to
;be normalized.
(define (normalize-constant li)
  (define (norm-const li const)
    (cond ((null? li) const)
          ((assoc (var-name (get-next-var li))
                  source);returns true if value exists
           (norm-const (cdr li)
                     (update-const-norm const 
                                (get-normalized-constant
                                 (get-normalized-value
                                  (var-name
                                   (get-next-var li))))
                                (var-exponent
                                 (get-next-var li)))))
          (else (norm-const
                 (cdr li)
                 const))));already normalized
  (norm-const (remove-constant li) (get-constant li)))

;returns a normalized list with an exponent
(define (normalize-all li)
  (cons (normalize-constant li) (normalize-vars li)))

;this is a procedure that merges the constants such
;that only unique variables exists.  This means there
;is only one instance of any variable in a normalized
;list which makes it easier to compare two normalized
;lists
;(merge-exponents updated-variable li)
;recursions are either
;(merge-exponents (update updated-variable)
;                  (remove-next-var li))
;or
;(cons updated-variable
;     (merge-exponents updated-variable
;         (cdr li))

;this checks to see if there is an instance of a
;given variable
(define (another-var? var li)
  (cond ((null? li) #f)
        ((eqv? (var-name var) (var-name (car li)))
         #t)
        (else
         (another-var? var (cdr li)))))

;a procedure that returns the next instance of a
;variable in a list.  It assumes one exists
(define (next-inst var li)
  (cond ((eqv? (var-name var) (var-name (car li)))
         (car li))
        (else
         (next-inst var (cdr li)))))
         
;a procedure that alters an exponent based on
;another exponent
(define (alt-exp var1 var2)
  (cons (var-name var1)
        (cons (+ (var-exponent var1)
                 (var-exponent var2))
              '())))

;a procedure that removes the next instance of a
;variable from a list
(define (rem-next-inst var li)
  (cond ((eqv? (var-name var) (var-name (car li)))
         (cdr li))
        (else
         (cons (car li)
               (rem-next-inst var (cdr li))))))

;if there is another update, then update the var
;if not then return the list with the updated first
;exponents, you still have to loop through the rest of
;the vars, this only merges the next exponent
(define (merge-exponents li)
  (define (merger updated-var rest-of-li)
    (cond ((another-var? updated-var rest-of-li)
           (merger (alt-exp
                    updated-var
                    (next-inst updated-var
                               rest-of-li))
                   (rem-next-inst updated-var
                             rest-of-li)))
          (else
           (cons updated-var
                 rest-of-li))))
  (merger (car li)
          (cdr li)))

;determines if every variable is unique
;starts at the car then checks every
;other pair, and then cdr's down the list in
;this fashion.  Assumes the constant has been
;removed
(define (unique-vars? li)
  (define (unique-iter lia lib)
    (cond ((= (length lia) 1) #t);no duplicates
          ((null? lib) (unique-iter (cdr lia) (cddr lia)))
          ((eqv? (var-name (car lia))
                 (var-name (car lib)))
           #f)
          (else
           (unique-iter lia (cdr lib)))))
  (unique-iter li (cdr li)))

;checks if the first var in the list is unique
(define (next-unique-var? li)
  (define (next-iter var rest)
    (cond ((null? rest) #t)
          ((eqv? (var-name var)
                 (var-name (car rest)))
           #f)
          (else
           (next-iter var (cdr rest)))))
  (next-iter (car li) (cdr li)))

;this serves as an outer loop list builder for
;the inner loop, merge-exponents
(define (main-merge li)
  (cond ((unique-vars? li) li)
        ((next-unique-var? li)
         (cons (car li);skips to the next
               (main-merge;var
                (merge-exponents (cdr li)))))
        (else
         (main-merge
          (merge-exponents li)))))

;goes through the entire list
(define (merge-all-exps li)
  (cons (get-constant li)
        (main-merge (cdr li))))

;assumes all the variables are unique
;and normalized
(define (compatible? lia lib)
  (define (com-iter a b b2)
    (cond ((null? a) #t)
          ((null? b) #f);can't find a match
          ((and (eqv? (var-name (car a))
                      (var-name (car b)))
                (eqv? (var-exponent (car a))
                      (var-exponent (car b))))
           (com-iter (cdr a) b2 b2))
          (else
           (com-iter a (cdr b) b2))))
  (com-iter (remove-constant lia)
            (remove-constant lib)
            (remove-constant lib)))

(define (convert a b)
  (cond ((compatible? (merge-all-exps (normalize-all a))
                      (merge-all-exps (normalize-all (cons 1 b))))
         (cons (/ (get-constant (merge-all-exps (normalize-all a)))
                  (get-constant (merge-all-exps (normalize-all (cons 1 b)))))
              (main-merge  b)))
        (else
         (display "These are not compatible")
         (newline))))
        
              


;(display (normalize-vars to))
;(display "\n")
;(display (normalize-constant to))
;(display "\n")
;(display (normalize-all to))
;(display "\n")
;(display (alt-exp '(sec 3)
;          (next-inst '(sec 3)
;                       (cdr (normalize-all to)))))
;(display "\n")
;(display (rem-next-inst '(sec 3)
;                        (cdr (normalize-all to))))
;(display (merge-all-exps '(5 (sec 5) (m 2) (sec 3) (m 1) (sec -2))))
;(display "\n")
;(display (next-unique-var? '((sec 5) (m 2) (sec 10) (ro 16) (g 3))))
;(display "\n")
;(display (compatible? '(5 (sec 2) (m 5) (ro 27)) '(5 (sec 2) (m 5) (p 27))))
;(display (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1))))
;(convert '(27.5 (mi 1)(hr -2)) '((furlong 1)(fortnight -1)(fortnight -1)))

;(display "\n")
;(display (mod-exp '((sec 2) (foo 5) (m 1)) 3))
;(display (convert '(27.5 (furlong 1)(fortnight -1)) '((hr -1) (mi 1))))
;(convert '(1 (kg 3) (m 3) (sec -6)) '((N 2)))
;(convert '(1 (kg 3) (m 3) (sec -6)) '((N 3)))

;THIS WORKS
;check to see if you can find the final value by
;utilizing, a U = (au/v) V
;(display (normalize-all '(27.5 (mi 1) (hr -1))))
;(display "\n")
;(display (normalize-all '(1 (furlong 1) (fortnight -1))))
;(display "\n")
;(display (/ (car (normalize-all '(27.5 (mi 1) (hr -1))))
;            (car (normalize-all '(1 (furlong 1) (fortnight -1))))))
;THIS WORKS
