; Raza Agha, Huvra Mehta, Peter Fedrizzi

; Implements: variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.
; OPERATORS: +,-,*,/,% (including the unary -)
; COMPARISON: ==, !=, <, >, <=, >=
; BOOLEAN: &&, ||, !

; Variables may store values of type int as well as true and false.
; Not necessary: Error checking on type
; Not necessary: short-circuit evaluation of && or ||

;hello

; I spent alot of time making set_state_variable to work correctly. 


(require "simpleParser.scm")

(parser "firsttest.java")

(define state '())

(define M_value_int
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value_int (operand1 
expression state)) (M_value_int (operand2 expression state))))
      ((eq? '- (operator expression)) (- (M_value_int (operand1 
expression state)) (M_value_int (operand2 expression state))))
      ((eq? '* (operator expression)) (* (M_value_int (operand1 
expression state)) (M_value_int (operand2 expression state))))
      ((eq? '/ (operator expression)) (quotient (M_value_int (operand1 
expression state)) (M_value_int (operand2 expression state))))
      ((eq? '% (operator expression)) (remainder (M_value_int (operand1 
expression state)) (M_value_int (operand2 expression state))))
      (else (error 'badoperator "illegal arithmetic expression")))))


;This function takes a variable and finds the value associated with it on the state.

(define get_state_variable
  (lambda (n state)
    (cond
      ((number? n) n)
      ((null? state) (error 'badoperator "variable not found")) ;
      ((eq? (caar state) n)   (caadr state) )
      (else (get_state_variable n (cons (cdar state) (list (cdadr state))))))))

;This function acts like assignment. It takes a variable and value and the state and returns the new state.
;If the variable is already present it changes the value of that variable.
; remember state is like ((a b c) (10 11 12))

; Takes old state and using variables returns the new one.
(define set_state_variable
  (lambda (var n state)
    (cond
      ((null? state) (append state  (cons (list var) (list (list n))))) ;this is only when state is empty otherwise hopefully this will never be called.
      ((and (null? (cdar state)) (not (eq? (caar state) var)))  ;I'm hoping this is the terminating condition when state has only one last element and that is not equal to variable we are inserting
       (cons (cons var (car state)) (list (cons n (cadr state ))))) ;then the new variable is appended to the state in the right place
      ((eq? (caar state) var)   (cons (car state) (list (append (list n) (cddr state)))) ) ;if it's the equal then change the associated value
      (else (append_two_lists (cons (list (caar state)) (list (list (caadr state)))) (set_state_variable var n (cons (cdar state) (list (cdadr state))))
      ))
      )))
;the last statement puts the first variable from state and it's value on the stack and later on appends it the new state.
;I create my own append function because it has to append things like ((v) (10)) ( (a b c) (10 11 12) ) for example.

(define append_two_lists
  (lambda (lis1 lis2)
    (cons (append (car lis1) (car lis2)) (list (append (cadr lis1) (cadr lis2))))))


; Trying to make a declaration type function wtih '@ as a placeholder. Might be incorrect.
(define declare_state_variable
  (lambda (var state)
    (cond
      ((null? state) (append state  (cons (list var) (list (list '@)))))
      ((eq? (caar state) var) (cons (list var) (cdr state)))
      (set_state_variable var n (cdr state)))))


(define operator
  (lambda (expression)
    (car expression)))

(define operand1   ;operand1 and operand2 both call get_state_variable 
  (lambda (expression state)
    (car (get_state_variable (cadr expression) state))))

(define operand2
  (lambda (expression state)
    (car (get_state_variable (caddr expression) state))))
