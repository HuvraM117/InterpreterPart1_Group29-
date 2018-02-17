; Raza Agha, Huvra Mehta, Peter Fedrizzi

; Implements: variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.
; OPERATORS: +,-,*,/,% (including the unary -)
; COMPARISON: ==, !=, <, >, <=, >=
; BOOLEAN: &&, ||, !

; Variables may store values of type int as well as true and false.
; Not necessary: Error checking on type
; Not necessary: short-circuit evaluation of && or ||

;hello

; I'm trying to get the first two statements (var x) (= x 10) to work properly. 

(require "simpleParser.scm")

(parser "firsttest.java")

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


;This function takes a variable and finds the value associated with it on the state. 
(define get_state_variable
  (lambda (n state)
    (cond
      ((number? n) n)
      ((or (null? state) (null? (cdr state))) (error 'badoperator "variable not found"))
      ((eq? (caar state) n)
       (cadr state))
      (get_state_variable (n (cdr state))))))

;This function acts like assignment. It takes a variable and value and the state and returns the new state.
;If the variable is already present it changes the value of that variable. Since it return the state, sometimes
;it would need to be called in place of state in the function we are using it.
(define set_state_variable
  (lambda (var n state)
    (cond
      ((null? state) (append state  (cons (list var) (list (list n)))))
      ((eq? (caar state) var) (cons (list var) (cdr state)))
      (set_state_variable var n (cdr state)))))

; Trying to make a declaration type function.
(define declare_state_variable
  (lambda (var state)
    (append state  (cons (list var) (list (list '@))))))


(define operator
  (lambda (expression)
    (car expression)))

(define operand1
  (lambda (expression)
    (get_state_variable (cadr expression) state)))

(define operand2
  (lambda (expression)
    (get_state_variable (caddr expression) state)))






            


