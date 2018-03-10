; Interpreter Project - Part 1
; GROUP 29:
; Raza Agha, Huvra Mehta, Peter Fedrizzi
; raa117, hsm20, pef21

(require racket/trace)

(require "simpleParser.scm")
;(parser "test.java")

;;;;;;;;;;;

; INTERPRET
(define interpret
  (lambda (filename)
    (return_type (get_mother_of_state 'return (M_state_main (parser filename) '(((return) (null))) (lambda (v) v)))))) ; initialize state

; Return the proper value
(define return_type
  (lambda (x)
    (cond
      ((equal? #t x) 'true)
      ((equal? #f x) 'false)
      ((number? x) x)
      (else (error "Return type unknown")))))

;;;;;;;;;;;

; MAIN
(define M_state_main
  (lambda (parselist S return) ; takes a parse tree and state
    (cond
      ((null? parselist) (return S)) ; end of parse tree
      ((not (list? parselist)) ( return (error "Input cannot be atom"))) ; (M_state_main 'var '()) => error
      ((not (list? (firstlist parselist))) (return (error "Input cannot be single list"))) ; (M_state_main '(var a) '()) => error
      ((equal? 'return (command parselist)) (M_state_main (nextlist parselist) (M_state_return (firstlist parselist) S return) return)) ; return
      
      ((equal? 'var (command parselist)) (M_state_main (nextlist parselist) (M_state_var (firstlist parselist) S return) return)) ; variable 
      ((equal? '= (command parselist)) (M_state_main (nextlist parselist) (M_state_assign (firstlist parselist) S return) return)) ; assignment
      ((equal? 'if (command parselist)) (M_state_main (nextlist parselist) (M_state_if (firstlist parselist) S return) return)) ; if
      ((equal? 'while (command parselist)) (M_state_main (nextlist parselist) (M_state_while (firstlist parselist) S return) return)) ; while

      ;do new stuff
      ((equal? 'begin (command parselist)) (M_state_main (nextlist parselist) (M_state_block (firstlist parselist) S return) return)); creates new layer
      ((equal? 'break (command parseList)) (M_state_main (nextlist parselist) (M_state_break (firstlist parselist) S return) return)); break
      ((equal? 'continue (command parseList)) (M_state_main (nextlist parselist) (M_state_continue (firstlist parselist return) S) return)); continue
      ((equal? 'throw (command parseList)) (M_state_main (nextlist parselist) (M_state_throw (firstlist parselist) S return) return)); throw
      ((equal? 'try (command parseList)) (M_state_main (nextlist parselist) (M_state_try (firstlist parselist) S return) return)); try
      
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

; RETURN
(define M_state_return ; only update return if it has a value of null
  (lambda (statement S return)
    (cond
      ((null? statement) (return (error "Parser is broken?")))
      ((equal? 'null (get_mother_of_state 'return S)) (return (set_mother_reassign 'return (M_expression (return_expression statement) S) S)))
      (else (return S)))))

; Abstractions for M_state_return
; Retrieves the expression of a return statement;
(define return_expression
  (lambda (statement)
    (cadr statement)))

;;;;;;;;;;;

; VARIABLE
(define M_state_var
  (lambda (statement S return)
    (cond
      ((null? statement) (return (error "Parser is broken")))
      ((null? (var_list statement)) (return (set_mother_declare (varname statement) '? S)))
      (else (return (set_mother_declare (varname statement) (M_expression (var_expression statement) S) S))))))

(trace M_state_var)

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

; ASSIGNMENT
(define M_state_assign
  (lambda (statement S return)
    (cond
      ((null? statement) (return (error "Parser is broken")))
      ((number? (get_mother_of_state (varname statement) S)) (return (set_mother_reassign (varname statement) (M_expression (assign_expression statement) S) S)))
      (else (return (set_mother_declare (varname statement) (M_expression (var_expression statement) S) S))))))

; Abstractions for M_state_assign
; Variable name
(define varname
  (lambda (statement)
    (cadr statement)))

; Retrieves the expression assigned to variable
(define assign_expression
  (lambda (statement)
    (caddr statement)))

(trace M_state_assign)

;;;;;;;;;;;
    
; EXPRESSION - Evaluates expression
(define M_expression
  (lambda (express S)
    (cond
      ((null? express) (error "We are broken"))
      ((and (not (list? express)) (equal? express 'true)) #t) ; true = #t
      ((and (not (list? express)) (equal? express 'false)) #f) ; false = #f
      ((and (not (list? express)) (number? express)) express) ; number
      ((and (not (list? express)) (equal? (get_mother_of_state express S) '?)) (error "Variable value not assigned")) ; variable value missing
      ((not (list? express)) (get_mother_of_state express S)) ; variable value
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

; IF
(define M_state_if
  (lambda (statement S return)
    (if (null? (potentialElse statement)) ; abstraction needed?
        (if (M_expression (condition statement) S)
            (M_state_main (list (then statement)) S return)
            (return S))
        (if (M_expression (condition statement) S)
            (M_state_main (list (then statement)) S return) ; make a list of lists?
            (M_state_main (list (else statement)) S return)))))

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


; WHILE
(define M_state_while
  (lambda (statement S return)
    (call/cc
     (lambda (break)
       (M_state_while_helper statement S return break)))))

(define M_state_while_helper
  (lambda (statement S return break)
    (cond
      ((M_expression (condition statement) S)
       (lambda (v)
         (if v
             (lambda (S2) (M_state_while_helper (list (then statement)) S2 return break)) ;call the the then statement on a second instance of state 
             break)
      (else (return S)))))))

;;;;;;;;;;;

;BLOCK


(define M_state_block
  (lambda (statement S return)
    (cond
      ((null? statement) (return (error "messed up")))
      (else (M_state_main (cdr statement) (addLayer S) return) (addLayer S)))))

(trace M_state_block)
(trace M_state_main)

(define addLayer
  (lambda (S)
    (cons (list '() '()) S)))

(define removeLayer
  (lambda (S)
    (cadr S)))

(trace addLayer)
(trace removeLayer)

;;;;;;;;;;;

; THROW
(define M_state_throw
  (lambda (statement S return)
    (return (set_mother_of_state 'throw (exceptionType statement) S))))

;abstraction
(define exceptionType
  (lambda (statement)
    (cadr statement)))

; TRY - CATCH - FINALLY

(define M_state_try
  (lambda (statement S return)
    (cond
      ((null? statement) (return (error "why tho?")))
      ((equals? (length statement) 4)
       (addLayer S (lambda (v)
                     (M_state_main (first_part statement) (lambda (v1)
                                                            (M_catch (second_part statement) v1 (lambda (v2 )
                                                                                                  (M_finally (second_part statement) v3 (lambda (v3)
                                                                                                                                          (removeLayer v3 (lambda (v4) (return v4))))))))))))
      ((equals? (length statement) 2) (M_state_block statement S return))
      ((equals? (direction statement) 'catch)
       (addLayer S (lambda (v)
                     (M_state_main (first_part statement) (lambda (v1)
                                                            (M_catch (second_part statement) v1 (lambda (v2 )
                                                                                                  (removeLayer v2 (lambda (v3) (return v3))))))))))
      ((equals? (direction statement) 'finally)
       (addLayer S (lambda (v)
                     (M_state_main (first_part statement) (lambda (v1)
                                                            (M_finally (second_part statement) v1 (lambda (v2)
                                                                                                    (removeLayer v2 (lambda (v3) (return v3))))))))))
      (else (error "this project is insane")))))

(define M_catch
  (lambda (statement S return)
    ((equal? (exc statement) (get_mother_of_state 'throw S)) (M_state_main (caught statement) S return))))

(define M_finally
  (lambda (statement S return)
    ((M_state_block statement S return))))

(define caught
  (lambda (catch_statement)
    (caaadr catch_statement)))
  
(define exc
  (lambda (catch_statement)
    (caadr catch_statement)))

(define first_part
  (lambda (try_statement)
    (cadr try_statement)))

(define second_part
  (lambda (try_statement)
    (caddr try_statement)))

(define final_part
  (lambda (try_statement)
    (cadddr try_statement)))

(define direction
  (lambda (try_statement)
    (caaddr try_statement)))
           
;;;;;;;;;;;

;BREAK
(define M_state_break
  (lambda (S return)
    (removeLayer S (lambda (v) v))))

;CONTINUE
(define M_state_continue
  (lambda (statement S return stop)
    (stop (M_state_Main statement S return))))


;;;;;;;;;;;

 
; STATE HANDLING

;(define set_mother_state

; This is for new values.

(define set_mother_declare
    (lambda (var value S)
      (cons (set_state_variable var value (car S)) (cdr S))))

; set_mother_reassign is for values already in the list

(define set_mother_reassign
  (lambda (var value S)
    (cond
      ((null? S) '())
      ((if_variable_there var (car S)) (cons (set_state_variable var value (car S)) (cdr S)))
      (else (cons (car S) (set_mother_reassign var value (cdr S )) )))))

(trace set_mother_declare)

;it will return 2 for f if state is (( (f g h) (2 1 3)) ((a b) (10 11))))
(define get_mother_of_state
  (lambda (v state)
    (cond
      ((state_empty state) (error "Variable not found"))
      ((if_variable_there v (car state)) (get_state_variable v (car state)))
       (else (get_mother_of_state v (cdr state))))))

(define state '(( (f g h) (2 1 3)) ((a b) (10 11))))

(define if_variable_there
  (lambda (var S)
    (cond
      ((null? (car S)) #f)
      ((eq? (caar S) var) #t) ; if variable is found return value
      (else (if_variable_there var (cons (cdar S) (list (cdadr S))))) ))) ; reduce copy of state recrusively

;Checks for empty state on list of states like '(( () ()) (() ()))
(define state_empty
  (lambda (S)
    (cond
    ((not (null? (caar S))) #f)
    ((and (null? (caar S)) (null? (cdr S))) #t)
    (else (state_empty (cdr S))))))

; CPS? This function takes a variable and finds the value associated with it within the state.
(define get_state_variable
  (lambda (var S)
    (cond
      ((number? var) var) ; not necessary?
      ((null? (car S)) (error "Variable not found"))
      ((eq? (caar S) var) (caadr S)) ; if variable is found return value
      (else (get_state_variable var (cons (cdar S) (list (cdadr S)))))))) ; reduce copy of state recrusively

(trace get_state_variable)
(trace get_mother_of_state)

; This function acts like assignment. It takes a variable and value and the state and returns the new state.
; If the variable is already present it changes the value of that variable. Remember state is like ((a b c) (10 11 12)).
(define set_state_variable
  (lambda (var value S)
    (cond
      ((null? S) (append S (cons (list var) (list (list value))))) ; when state is empty, it creates the state
      ((and (null? (car S)) (null? (cadr S) )) (cons (list var) (list (list value))))
      ((and (null? (cdar S)) (not (eq? (caar S) var))) ; terminating condition when the state has only one last element and that is not equal to the input variable
       (cons (cons var (car S)) (list (cons value (cadr S))))) ; then the new variable is appended to the state in the right place
      ((eq? (caar S) var) (cons (car S) (list (append (list value) (cdadr S))))) ; if input variable is present then change the associated value
      (else (append_two_lists (cons (list (caar S)) (list (list (caadr S)))) (set_state_variable var value (cons (cdar S) (list (cdadr S)))))))))

(trace set_state_variable)
; the last statement puts the first variable from state and it's value on the stack and later on appends it to the new state.
; I create my own append function because it has to append things like '((a b c) (10 11 12) => ("fake cons" '((a) (10)) '((b c)(11 12))) for example.

(define append_two_lists ; "fake cons"
  (lambda (lis1 lis2)
    (cons (append (car lis1) (car lis2)) (list (append (cadr lis1) (cadr lis2))))))

(trace M_expression)

; Abstractions for state handling ???

;;;;;;;;;;;

; TESTS, for Part 2

; Not implemented: side effects
; Not implemented: error checking on type
; Not implemented: error checking for redefine
; Not implemented: short-circuit evaluation of && or ||

(parser "test1.java")
(interpret "test1.java") ; => 20
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