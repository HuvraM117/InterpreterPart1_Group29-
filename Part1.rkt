; Interpreter Project - Part 1
; Raza Agha, Huvra Mehta, Peter Fedrizzi
;comment

(require "simpleParser.scm")
(parser "test.java")

; INTERPRET
(define interpret
  (lambda (filename)
    (M_state_main (parser filename) '((return) (null)))))

; MAIN
(define M_state_main
  (lambda (parselist S) ; takes a parse tree and state
    (cond
      ; this should have a helper that searches for the return variable!
      ((null? parselist) (get_state_variable 'return S)) ; (error "Finished parsing, no return statement detected!")) ; (M_state_main '() '()) => error
      ((not (list? (car parselist))) (error "Mistake! Fix M_state_main list handling...")) ; (M_state_main '(var a) '()) => error
      ((equal? 'return (command parselist)) (M_state_main (cdr parselist) (M_state_return (firstlist parselist) S))) ; return
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

; STATE FOR LIST - 2
(define M_state_list
  (lambda (statementlist S) ; takes a parse tree and state

        
      ; this should have a helper that searches for the return variable!
      ((null? statementlist) S)
      
      ((equal? 'return (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_return (piece1 statementlist) S))) ; return
      ((equal? 'var (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_var (piece1 statementlist) S))) ; variable 
      ((equal? '= (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_assign (piece statementlist) S))) ; assignment
      ((equal? 'if (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_if (firstlist statementlist) S))) ; if firstlist
      ((equal? 'while (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_while (firstlist statementlist) S))) ; while statement
      (else (error "What is going on?"))))

; STATE FOR LIST
;(define M_state_list
;  (lambda (statementlist S) ; takes a parse tree and state
;    (if ((null? statementlist) S)
;        (if (list? (piece1 statementlist))
;            ())
;        (if (list? (piece2 statementList))
;            ())
;        (else ()))))
;        
;      ; this should have a helper that searches for the return variable!
;      ((null? statementlist) S)
;      
;      ((equal? 'return (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_return (piece1 statementlist) S))) ; return
;      ((equal? 'var (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_var (piece1 statementlist) S))) ; variable 
;      ((equal? '= (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_assign (piece statementlist) S))) ; assignment
;      ((equal? 'if (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_if (firstlist statementlist) S))) ; if firstlist
;      ((equal? 'while (statecommand statementlist)) (M_state_list (cdr statementlist) (M_state_while (firstlist statementlist) S))) ; while statement
;      (else (error "What is going on?")))))

; Abstractions for Main
; Retrieves command in next statement of parse tree
(define statecommand
  (lambda (statementlist)
    (car statementlist)))

(define piece1
  (lambda (statementlist)
    (cadr statementlist)))

(define piece2
  (lambda (statementlist)
    (caddr statementlist)))
          

; !!! - I think we need to change how states are passed for "if" and "while" parts - !!! 

;;;;;;;;;;;

; RETURN
(define M_state_return ; store a return variable and only update if it is still null
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken"))
      ((equal? 'null (get_state_variable 'return S)) (set_state_variable 'return (M_expression (return_expression statement) S) S))
      (else S)))) ; not necessary?
      ;((list? (return_expression statement)) (set_state_variable 'return (M_expression (return_expression statement) S) S)) ; change #t to #f for the return
      ;(else (error "why?"))))) ;(get_state_variable (return_expression statement) S))))) ; not necessary?

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
      ((equal? operator (car oplist)) #t)
      (else (member? operator (cdr oplist))))))

;;;
    
; BOOLEAN EXPRESSION - EVALUATION
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

; VALUE EXPRESSION - EVALUATION
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
    (if (null? (cdddr statement)) ; abstraction needed?
        (if (M_expression (condition statement) S)
            (M_state_main (then statement) S))
        (if (M_expression (condition statement) S)
        (M_state_main (then statement) S) ; make a list of lists?
        (M_state_main (else statement) S)))))

; Abstractions for if-statement
(define condition
  (lambda (if_statement)
    (cadr if_statement)))

(define then ; Also used for while loop
  (lambda (if_statement)
    (caddr if_statement)))

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
      ((null? S) (error "Variable not found"))
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

;;;;;;;;;;;

; TESTS

; Not implemented: error checking on type
; Not implemented: side effects
; Not implemented: short-circuit evaluation of && or ||
; Not implemented: true instead of #t
; Not implemented: proper return structure

(parser "test1.java")
(interpret "test1.java") ; => 150
(parser "test2.java")
(interpret "test2.java") ; => -4
(parser "test3.java")
(interpret "test3.java") ; => 10
(parser "test4.java")
(interpret "test4.java") ; => 16
(parser "test5.java")
(interpret "test5.java") ; => 220

; PROBLEM: M_state_main cannot handle '(= x 3) only '((= x 3)(return 'something'))
(parser "test6.java")
(interpret "test6.java") ; => 5
;(parser "test7.java")
;(interpret "test7.java") ; => 6
;(parser "test8.java")
;(interpret "test8.java") ; => 10

; PROBLEM: Not sure.
;(parser "test9.java")
;(interpret "test9.java") ; => 5

(parser "test10.java")
(interpret "test10.java") ; return 6 * -(4 * 2) + 9; => -39

; PROBLEM: This code should return an error, because y has not been declared
;(parser "test11.java")
;(interpret "test11.java") ; => This code should give an error (using before declaring)

; PROBLEM: Assignment with another variable
;(parser "test12.java")
;(interpret "test12.java") ; => This code should give an error (using before declaring).

; PROBLEM: Error for adding '? to stuff
;(parser "test13.java")
;(interpret "test13.java") ; => This code should give an error (using before assigning).
;(parser "test14.java")
;(interpret "test14.java") ; => This code should give an error (redefining). This is not a required error, but it would be nice if you could catch these.

; PROBLEM: Fix this error simple error
(parser "test15.java")
(interpret "test15.java") ; => This code should return true (not #t).

;(parser "test16.java")
;(interpret "test16.java") ; => 100
;(parser "test17.java")
;(interpret "test17.java") ; => This code should return false (not #f).
;(parser "test18.java")
;(interpret "test18.java") ; => true
;(parser "test19.java")
;(interpret "test19.java") ; => 128
;(parser "test20.java")
;(interpret "test20.java") ; => 12


; Additional Tests for Students Looking for an Extra Challenge...
;(parser "test21.java")
;(interpret "test21.java") ; => 30
;(parser "test22.java")
;(interpret "test22.java") ; => 11
;(parser "test23.java")
;(interpret "test23.java") ; => 1106
;(parser "test24.java")
;(interpret "test24.java") ; => 12
;(parser "test25.java")
;(interpret "test25.java") ; => 16
;(parser "test26.java")
;(interpret "test26.java") ; => 72
;(parser "test27.java")
;(interpret "test27.java") ; => 21
;(parser "test28.java")
;(interpret "test28.java") ; => 164
