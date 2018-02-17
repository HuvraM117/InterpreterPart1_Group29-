; Raza Agha, Huvra Mehta, Peter Fedrizzi

; Implements: variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.
; OPERATORS: +,-,*,/,% (including the unary -)
; COMPARISON: ==, !=, <, >, <=, >=
; BOOLEAN: &&, ||, !

; Variables may store values of type int as well as true and false.
; Not necessary: Error checking on type
; Not necessary: short-circuit evaluation of && or ||

;hello

(require "simpleParser.scm")

(parser "firsttest.java")

; Raza Agha, Huvra Mehta, Peter Fedrizzi

; Implements: variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.
; OPERATORS: +,-,*,/,% (including the unary -)
; COMPARISON: ==, !=, <, >, <=, >=
; BOOLEAN: &&, ||, !

; Variables may store values of type int as well as true and false.
; Not necessary: Error checking on type
; Not necessary: short-circuit evaluation of && or ||

;hello

(require "simpleParser.scm")

(parser "test.java")

(define state '())

(define M_value_int
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value_int (operand1 
expression)) (M_value_int (operand2 expression))))
      ((eq? '- (operator expression)) (- (M_value_int (operand1 
expression)) (M_value_int (operand2 expression))))
      ((eq? '* (operator expression)) (* (M_value_int (operand1 
expression)) (M_value_int (operand2 expression))))
      ((eq? '/ (operator expression)) (quotient (M_value_int (operand1 
expression)) (M_value_int (operand2 expression))))
      ((eq? '% (operator expression)) (remainder (M_value_int (operand1 
expression)) (M_value_int (operand2 expression))))
      (else (error 'badoperator "illegal arithmetic expression")))))

(define get_state_variable
  (lambda (n state)
    (cond
      ((number? n) n)
      ((or (null? state) (null? (cdr state))) (error 'badoperator "variable not found"))
      ((eq? (caar state) n)
       (cadr state))
      (get_state_variable (n (cdr state))))))


(define set_state_variable
  (lambda (var n state)
    (cond
      ((null? state) (append state  (cons (list var) (list (list n)))))
      ((eq? (caar state) var) (cons (list var) (cdr state)))
      (set_state_variable var n (cdr state)))))
      

(define operator
  (lambda (expression)
    (car expression)))

(define operand1
  (lambda (expression)
    (get_state_variable (cadr expression) state)))

(define operand2
  (lambda (expression)
    (get_state_variable (caddr expression) state)))




            


