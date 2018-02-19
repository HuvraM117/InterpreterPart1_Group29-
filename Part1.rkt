; Interpreter Project - Part 1
; Raza Agha, Huvra Mehta, Peter Fedrizzi

(require "simpleParser.scm")
(parser "test.java")

; MAIN
(define M_state_main
  (lambda (parselist S) ; takes a parse tree and state
    (cond
      ((null? parselist) (error "Finished parsing, no return statement detected!")) ; (M_state_main '() '()) => error
      ((not (list? (car parselist))) (error "Mistake! Fix M_expression...")) ; (M_state_main '(var a) '()) => error
      ((equal? 'return (command parselist)) (M_state_return (firstlist parselist) S)) ; return
      ((equal? 'var (command parselist)) (M_state_main (cdr parselist) (M_state_var (firstlist parselist) S))) ; variable
      ((equal? '= (command parselist)) (M_state_main (cdr parselist) (M_state_assign (firstlist parselist) S))) ; assignment
      ((equal? 'if (command parselist)) (M_state_main (cdr parselist) (M_state_if (firstlist parselist) S))) ; if firstlist
      ((equal? 'while (command parselist)) (M_state_main (cdr parselist) (M_state_while (firstlist parselist) S))) ; while statement
      (else (error "What is going on?")))))

; Abstractions for Main
; Retrieves command in next statement of parse tree
(define command
  (lambda (parselist)
    (caar parselist)))

; Retrieves the next statement of parse tree
(define firstlist ;(statement '((return 150) (return 120))) => (return 150)
  (lambda (parselist)
    (car parselist)))

;;;;;;;;;;;

; RETURN
(define M_state_return
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken"))
      ((number? (return_expression statement)) (return_expression statement)) ; not necessary?
      ((list? (return_expression statement)) (M_expression (return_expression statement) S))
      (else (get_state_variable (return_expression statement) S))))) ; not necessary?

; Retrieves the expression of a return statement;
(define return_expression
  (lambda (statement)
    (cadr statement)))

;;;;;;;;;;;

; VARIABLE
(define M_state_var
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken"))
      ((null? (cddr statement)) (set_state_variable (varname statement) '? S))
      (else (set_state_variable (varname statement) (M_expression (var_expression statement) S) S)))))

; Variable name
(define varname
  (lambda (statement)
    (cadr statement)))

; Retrieves the expression assigned to variable
(define var_expression
  (lambda (statement)
    (caddr statement)))

;;;;;;;;;;;

; ASSIGNMENT
(define M_state_assign
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parse is broken"))
      (else (set_state_variable (varname statement) (M_expression (assign_expression statement) S) S)))))

; Variable name
(define varname
  (lambda (statement)
    (cadr statement)))

; Retrieves the expression assigned to variable
(define assign_expression
  (lambda (statement)
    (caddr statement)))

;;;;;;;;;;;
    
; EXPRESSION - EVALUATION
(define M_expression
  (lambda (express S)
    (cond
      ((null? express) (error "We are broken"))
      ((and (not (list? express)) (equal? express 'true)) #t) ; true = #t
      ((and (not (list? express)) (equal? express 'false)) #f) ; false = #f
      ((and (not (list? express)) (number? express)) express) ; number
      ((not (list? express)) (get_state_variable express S)) ; variable value
      ((member? (operator express) '(+,-,*,/,%)) (M_value express S)) ; send list to M_value
      ((member? (operator express) '(!,&&,||,!=,==,<,>,<=,>=)) (M_boolean express S)) ; send list to M_boolean
      (else (error "It's getting down here when it shouldn't have"))))) ; not necessary?

; Determines if operator is for boolean or int (helper function)
(define member?
  (lambda (operator oplist)
    (cond
      ((null? oplist) #f)
      ((equal? operator (car oplist)) #t)
      (else (member? operator (cdr oplist))))))

;;;
    
; BOOLEAN EXPRESSION - EVALUATION
(define M_boolean
  (lambda (express S)
    (cond
      ((null? express) (error "M_expression is broken"))
      ((equal? '! (operator express)) (not (M_expression (exp1 express) S)))
      ((equal? '&& (operator express)) (and (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '|| (operator express)) (or (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '!= (operator express)) (not (equal? (M_expression (exp1 express) S) (M_expression (exp2 express) S))))
      ((equal? '== (operator express)) (equal? (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '< (operator express)) (< (M_expression (exp1 express) S) (M_expression (exp2 express) S))) ;something
      ((equal? '> (operator express)) (> (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '<= (operator express)) (<= (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '>= (operator express)) (>= (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      (else (error "Missing operator, bad team!")))))

; VALUE EXPRESSION - EVALUATION
(define M_value
  (lambda (express S)
    (cond
      ((null? express) (error "M_expression is broken"))
      ((equal? '+ (operator express)) (+ (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '- (operator express)) (- (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '* (operator express)) (* (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '/ (operator express)) (quotient (M_expression (exp1 express)S) (M_expression (exp2 express)S)))
      ((equal? '% (operator express)) (remainder (M_expression (exp1 express)S) (M_expression (exp2 express)S)))
      (else (error "illegal arithmetic expression")))))

; Abstractions for evaluation functions
(define operator
  (lambda (express)
    (car express)))

; first operand
(define exp1
  (lambda (express)
    (cadr express)))

; second operand
(define exp2
  (lambda (express)
    (caddr express)))

;;;;;;;;;;;

; IF
(define M_state_if
  (lambda (statement S)
    (if (M_expression (condition statement) S)
        (M_state_main (then statement) S)
        (M_state_main (else statement) S))))

; Abstractions for if-statement
(define condition
  (lambda (if_statement)
    (cadr if_statement)))

(define then ; Also used for while loop
  (lambda (if_statement)
    (caddr if_statment)))

(define else
  (lambda (if_statement)
    (cadddr if_statement)))

;;;;;;;;;;;

; WHILE
(define M_state_while
  (lambda (statement S)
    (if (M_expression (condition statement) S)
        (M_state_while statement (M_state_main (then statement) S)))))

; Abstraction for while-statement
;(define then
;  (lambda (while_statement)
;    (caddr while_statment)))

;;;;;;;;;;;
 
; STATE HANDLING

; This function takes a variable and finds the value associated with it within the state.
(define get_state_variable
  (lambda (var S)
    (cond
      ((number? var) var) ; not necessary?
      ((null? S) (error "variable not found"))
      ((eq? (caar S) var) (caadr S)) ; if variable is found return value
      (else (get_state_variable var (cons (cdar S) (list (cdadr S)))))))) ; reduce copy of state recrusively

; Abstraction probably not necessary?

; This function acts like assignment. It takes a variable and value and the state and returns the new state.
; If the variable is already present it changes the value of that variable. Remember state is like ((a b c) (10 11 12)).
(define set_state_variable
  (lambda (var value S)
    (cond
      ((null? S) (append S (cons (list var) (list (list value))))) ; when state is empty, it creates the state
      ((and (null? (cdar S)) (not (eq? (caar S) var))) ; terminating condition when the state has only one last element and that is not equal to the input variable
       (cons (cons var (car S)) (list (cons value (cadr S))))) ; then the new variable is appended to the state in the right place
      ((eq? (caar S) var) (cons (car S) (list (append (list value) (cdadr S))))) ; if input variable is present then change the associated value
      (else (append_two_lists (cons (list (caar S)) (list (list (caadr S)))) (set_state_variable var value (cons (cdar S) (list (cdadr S)))))))))
; the last statement puts the first variable from state and it's value on the stack and later on appends it to the new state.
; I create my own append function because it has to append things like '((a b c) (10 11 12) => ("fake cons" '((a) (10)) '((b c)(11 12))) for example.

(define append_two_lists ; "fake cons"
  (lambda (lis1 lis2)
    (cons (append (car lis1) (car lis2)) (list (append (cadr lis1) (cadr lis2))))))

; Not implemented: error checking on type
; Not implemented: side effects
; Not implemented: short-circuit evaluation of && or ||

(parser "test1.java")
(M_state_main (parser "test1.java") '()) ; => 150
(parser "test2.java")
(M_state_main (parser "test2.java") '()) ; => -4
(parser "test3.java")
(M_state_main (parser "test3.java") '()) ; => 10
(parser "test4.java")
(M_state_main (parser "test4.java") '()) ; => 16
