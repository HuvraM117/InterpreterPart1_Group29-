; Interpreter Project - Part 2
; GROUP 29:
; Raza Agha, Huvra Mehta, Peter Fedrizzi
; raa117, hsm20, pef21

(require "simpleParser.scm")
;(parser "test.java")

;;;;;;;;;;;

; INTERPRET
(define interpret
  (lambda (filename)
    (return_type (get_state_variable 'return (M_state_main (parser filename) '((return) (null))))))) ; initialize state

; Return the proper value
(define return_type
  (lambda (x)
    (cond
      ((equal? #t x) 'true)
      ((equal? #f x) 'false)
      ((number? x) x)
      (else (error "Return type unknown")))))

;;;;;;;;;;;

; CPS? MAIN
(define M_state_main
  (lambda (parselist S) ; takes a parse tree and state
    (cond
      ((null? parselist) S) ; end of parse tree
      ((not (list? parselist)) (error "Input cannot be atom")) ; (M_state_main 'var '()) => error
      ((not (list? (firstlist parselist))) (error "Input cannot be single list")) ; (M_state_main '(var a) '()) => error
      ((equal? 'return (command parselist)) (M_state_main (nextlist parselist) (M_state_return (firstlist parselist) S))) ; return
      ((equal? 'var (command parselist)) (M_state_main (nextlist parselist) (M_state_var (firstlist parselist) S))) ; variable 
      ((equal? '= (command parselist)) (M_state_main (nextlist parselist) (M_state_assign (firstlist parselist) S))) ; assignment
      ((equal? 'if (command parselist)) (M_state_main (nextlist parselist) (M_state_if (firstlist parselist) S))) ; if
      ((equal? 'while (command parselist)) (M_state_main (nextlist parselist) (M_state_while (firstlist parselist) S))) ; while
      ;((equal? 'begin (command parselist)) (M_state_main (nextlist parselist) (M_state_block (firstlist parselist) S)))
      (else (error "Something bad happened, broken parser?")))))

; Abstractions for M_state_main
; Retrieves command in next statement of parse tree
(define command
  (lambda (parselist)
    (caar parselist)))

; Retrieves the next statement of parse tree
(define firstlist ; (statement '((return 150) (return 120))) => (return 150)
  (lambda (parselist)
    (car parselist)))

; Returns the subtree of given parse tree
(define nextlist
  (lambda (parselist)
    (cdr parselist)))
  
;;;;;;;;;;;

; CPS? RETURN
(define M_state_return ; only update return if it has a value of null
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken?"))
      ((equal? 'null (get_state_variable 'return S)) (set_state_variable 'return (M_expression (return_expression statement) S) S))
      (else S))))

; Abstractions for M_state_return
; Retrieves the expression of a return statement;
(define return_expression
  (lambda (statement)
    (cadr statement)))

;;;;;;;;;;;

; CPS? VARIABLE
(define M_state_var
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken"))
      ((null? (var_list statement)) (set_state_variable (varname statement) '? S))
      (else (set_state_variable (varname statement) (M_expression (var_expression statement) S) S)))))

; Abstractions for M_state_var
; Variable name
(define varname
  (lambda (statement)
    (cadr statement)))

; Retrieves the expression assigned to variable
(define var_expression
  (lambda (statement)
    (caddr statement)))

; Retrieves the expression list
(define var_list
  (lambda (statement)
    (cddr statement)))

;;;;;;;;;;;

; CPS? ASSIGNMENT
(define M_state_assign
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken"))
      ((number? (get_state_variable (varname statement) S))  (set_state_variable (varname statement) (M_expression (assign_expression statement) S) S))
      (else (set_state_variable (varname statement) (M_expression (assign_expression statement) S) S)))))

; Abstractions for M_state_assign
; Variable name
(define varname
  (lambda (statement)
    (cadr statement)))

; Retrieves the expression assigned to variable
(define assign_expression
  (lambda (statement)
    (caddr statement)))

;;;;;;;;;;; This does not need to pass a continuation.
    
; EXPRESSION - Evaluates expression
(define M_expression
  (lambda (express S)
    (cond
      ((null? express) (error "We are broken"))
      ((and (not (list? express)) (equal? express 'true)) #t) ; true = #t
      ((and (not (list? express)) (equal? express 'false)) #f) ; false = #f
      ((and (not (list? express)) (number? express)) express) ; number
      ((and (not (list? express)) (equal? (get_state_variable express S) '?)) (error "Variable value not assigned")) ; variable value missing
      ((not (list? express)) (get_state_variable express S)) ; variable value
      ((member? (operator express) '(+ - * / %)) (M_value express S)) ; send list to M_value
      ((member? (operator express) '(! && || != == < > <= >=)) (M_boolean express S)) ; send list to M_boolean
      (else (error "It's getting down here when it shouldn't have."))))) ; not necessary?

; Determines if operator is for boolean or int (helper function)
(define member?
  (lambda (operator oplist)
    (cond
      ((null? oplist) #f)
      ((equal? operator (car oplist)) #t) ; abstraction not applied because it operates on a list
      (else (member? operator (cdr oplist))))))
    
; BOOLEAN - Evaluates boolean expression
(define M_boolean
  (lambda (express S)
    (cond
      ((null? express) (error "M_expression is broken"))
      ((equal? '! (operator express)) (not (M_expression (exp1 express) S))) ; negates statement
      ((equal? '&& (operator express)) (and (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '|| (operator express)) (or (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '!= (operator express)) (not (equal? (M_expression (exp1 express) S) (M_expression (exp2 express) S))))
      ((equal? '== (operator express)) (equal? (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '< (operator express)) (< (M_expression (exp1 express) S) (M_expression (exp2 express) S))) ;something
      ((equal? '> (operator express)) (> (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '<= (operator express)) (<= (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      ((equal? '>= (operator express)) (>= (M_expression (exp1 express) S) (M_expression (exp2 express) S)))
      (else (error "Missing operator, bad team!")))))

; VALUE - Evaluates numerical expression
(define M_value
  (lambda (express S)
    (cond
      ((null? express) (error "M_expression is broken"))
      ((and (equal? '- (operator express)) (null? (cddr express))) (* -1 (M_expression (exp1 express) S))) ; negative of statement ; abstraction needed?
      ((equal? '+ (operator express)) (+ (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '- (operator express)) (- (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '* (operator express)) (* (M_expression (exp1 express) S) (M_expression (exp2 express)S)))
      ((equal? '/ (operator express)) (quotient (M_expression (exp1 express)S) (M_expression (exp2 express)S)))
      ((equal? '% (operator express)) (remainder (M_expression (exp1 express)S) (M_expression (exp2 express)S)))
      (else (error "illegal arithmetic expression")))))

; Abstractions for M_expression
; operator type
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

; CPS? IF
(define M_state_if
  (lambda (statement S)
    (if (null? (cdddr statement)) ; abstraction needed?
        (if (M_expression (condition statement) S)
            (M_state_main (list (then statement)) S)
            S)
        (if (M_expression (condition statement) S)
        (M_state_main (list (then statement)) S) ; make a list of lists?
        (M_state_main (list (else statement)) S)))))

; Abstractions for M_state_if
(define condition
  (lambda (if_statement)
    (cadr if_statement)))

(define then ; also used for while loop
  (lambda (if_statement)
    (caddr if_statement)))

(define else
  (lambda (if_statement)
    (cadddr if_statement)))

;;;;;;;;;;;

; CPS? WHILE
(define M_state_while
  (lambda (statement S)
    (if (M_expression (condition statement) S)
        (M_state_while statement (M_state_main (list (then statement)) S))
        S)))

;;;;;;;;;;;A
 
; STATE HANDLING

; CPS? This function takes a variable and finds the value associated with it within the state.
(define get_state_variable
  (lambda (var S)
    (cond
      ((number? var) var) ; not necessary?
      ((null? (car S)) (error "Variable not found"))
      ((eq? (caar S) var) (caadr S)) ; if variable is found return value
      (else (get_state_variable var (cons (cdar S) (list (cdadr S)))))))) ; reduce copy of state recrusively

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

; Abstractions for state handling ???

;;;;;;;;;;;

; TESTS, for Part 2

; Not implemented: side effects
; Not implemented: error checking on type
; Not implemented: error checking for redefine
; Not implemented: short-circuit evaluation of && or ||

;(parser "test1.java")
;(interpret "test1.java") ; => 20
;(parser "test2.java")
;(interpret "test2.java") ; => 164
;(parser "test3.java")
;(interpret "test3.java") ; => 32
;(parser "test4.java")
;(interpret "test4.java") ; => 2
;(parser "test5.java")
;(interpret "test5.java") ; => This code should give an error.
;(parser "test6.java")
;(interpret "test6.java") ; => 25
;(parser "test7.java")
;(interpret "test7.java") ; => 21
;(parser "test8.java")
;(interpret "test8.java") ; => 6
;(parser "test9.java")
;(interpret "test9.java") ; => -1
;(parser "test10.java")
;(interpret "test10.java") ; => 789
;(parser "test11.java")
;(interpret "test11.java") ; => This code should give an error.
;(parser "test12.java")
;(interpret "test12.java") ; => This code should give an error.
;(parser "test13.java")
;(interpret "test13.java") ; => This code should give an error.
;(parser "test14.java")
;(interpret "test14.java") ; => 12
;(parser "test15.java")
;(interpret "test15.java") ; => 125
;(parser "test16.java")
;(interpret "test16.java") ; => 110
;(parser "test17.java")
;(interpret "test17.java") ; => 2000400
;(parser "test18.java")
;(interpret "test18.java") ; => 101
;(parser "test19.java")
;(interpret "test19.java") ; => This code should give an error.

; Additional Tests for Students Looking for an Extra Challenge...
;(parser "test20.java")
;(interpret "test20.java") ; => 21ss