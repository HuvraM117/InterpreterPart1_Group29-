; Raza Agha, Huvra Mehta, Peter Fedrizzi

(require "simpleParser.scm")
(parser "test.java")

(parser "test1.java")
;(M_state_main (parser "test1.java") '()) ; => 150
(parser "test2.java")
;(M_state_main (parser "test2.java") '()) ; => -4
(parser "test3.java")
;(M_state_main (parser "test3.java") '()) ; => 10
(parser "test4.java")
;(M_state_main (parser "test4.java") '()) ; => 16

; Structure of main routine
(define M_state_main
  (lambda (parselist S) ; takes a parse tree and state
    (cond
      ((null? parselist) (error "Finished parsing, no return statement detected!")) ; (M_state_main '() '()) => error
      ((not (list? (car parselist))) (M_expression parselist S)) ; (M_state_main '(var a) '()) => error
      ;;; INCOMPLETE BELOW ;;;
      ((equal? 'return (command parselist)) (M_state_return (statement parselist) S)) ; return
      ((equal? 'var (command parselist)) 1) ; variable
      ((equal? '= (command parselist)) 2) ; assignment
      ((equal? 'if (command parselist)) 4) ; if statement
      ((equal? 'while (command parselist)) 5) ; while statement
      (else (error "What is going on? - Peter")))))

; Retrieves command in next statement of parse tree
(define command
  (lambda (parselist)
    (caar parselist)))

; RETURN - INCOMPLETE
(define M_state_return
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken - Peter"))
      ((number? (cadr statement)) (cadr statement))
      ((list? (caadr statement)) (cadr statement)) ; evaluate expression
      (else (get_state_variable (cadr statement))))));

; VARIABLE - INCOMPLETE
(define M_state_var ; return a new state
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parser is broken - Peter"))
      ((null? (cddr statement)) (set_state_variable (cadr statement) '? S)) ;abstract
      (else (set_state_variable (cadr statement) (caddr statement) S))))) ;evalute expression

; ASSIGNMENT - INCOMPLETE
(define M_state_assign
  (lambda (statement S)
    (cond
      ((null? statement) (error "Parse is broken -Peter")) ;abstract
      (set_state_variable (cadr statement) (caddr statement) S)))) ;evaluate expression 
    
;; Expression - INCOMPLETE
(define M_expression
  (lambda (statement S)
    (cond
      ((null? statement) (error "We are broken"))
      ((and (not (list? statement)) (equal? statement 'true)) #t)
      ((and (not (list? statement)) (equal? statement 'false)) #f)
      ((not (list? statement)) statement)
      ((member? (operator statement) '(+,-,*,/,%) (M_value statement S))) ; must be list
      ((member? (operator statement) '(!,&&,||,!=,==,<,>,<=,>=) (M_boolean statement S))) ; must be list
      (else (error "We are really broken")))))

(define member?
  (lambda (operator oplist)
    (cond
      ((null? oplist) #f)
      ((eq? operator (car oplist)) #t)
      (else (member? operator (cdr oplist))))))
    
; M_BOOLEAN
(define M_boolean
  (lambda (conditional S)
    (cond
      ((null? conditional) (error "Parser is broken"))
      ((equal? 'true (car conditional)) #t)
      ((equal? 'false (car conditional)) #f)
      ((equal? '&& (operator conditional))
       (and (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      ((equal? '|| (operator conditional))
       (or (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      ((equal? '! (operator conditional))
       (not (M_state_main (first conditional) S)))
      ((equal? '== (operator conditional))
        (equal? (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      ((equal? '!= (operator conditional))
        (not (equal? (M_state_main (first conditional) S) (M_state_main (second conditional) S))))
      ((equal? '< (operator conditional))
       (< (M_state_main (first conditional) S) (M_state_main (second conditional) S))) ;something
      ((equal? '> (operator conditional))
       (> (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      ((equal? '<= (operator conditional))
       (<= (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      ((equal? '>= (operator conditional))
       (>= (M_state_main (first conditional) S) (M_state_main (second conditional) S)))
      
      (else (error "Bad condtion - Peter")))))

; M_VALUE

(define M_value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value (operand1 
expression state)) (M_value (operand2 expression state))))
      ((eq? '- (operator expression)) (- (M_value (operand1 
expression state)) (M_value (operand2 expression state))))
      ((eq? '* (operator expression)) (* (M_value (operand1 
expression state)) (M_value (operand2 expression state))))
      ((eq? '/ (operator expression)) (quotient (M_value (operand1 
expression state)) (M_value (operand2 expression state))))
      ((eq? '% (operator expression)) (remainder (M_value (operand1 
expression state)) (M_value (operand2 expression state))))
      (else (error 'badoperator "illegal arithmetic expression")))))

(define M_value_int ; DELETE LATER MAYBE, WHO KNOWS. THIS IS A LIVING HELL
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

; INCOMPLETE BELOW ;

(define operator
  (lambda (command)
    (car command)))

(define second1
  (lambda (command)
    ;((null? command) (error "Null operand"))
    (cadr command)))

(define second2
  (lambda (command)
    (caddr command)))

  (define first1   ;operand1 and operand2 both call get_state_variable 
  (lambda (expression state)
    (car (get_state_variable (cadr expression) state))))

(define first2
  (lambda (expression state)
    (car (get_state_variable (caddr expression) state))))

; IF
(define M_state_if
  (lambda (statement S)
    (if (M_boolean (condition statement) S)
        (M_state_main (then statement) S)
        (M_state_main (else statement) S))))

(define condition
  (lambda (if_statement)
    (cadr if_statement)))

(define then
  (lambda (if_statement)
    (caddr if_statment)))

(define else
  (lambda (if_statement)
    (cadddr if_statement)))

; WHILE
(define M_state_while
  (lambda (statement S)
    (if (M_boolean (condition statement) S)
        (then (M_state_while (M_state_main (then statement) (M_state_main S)
        (M_state_main (else statement) S)))))))

;; ABSTRACT - UNIFIY THEM

(define operator
  (lambda (expression)
    (car expression)))

(define operand1   ;operand1 and operand2 both call get_state_variable 
  (lambda (expression state)
    (car (get_state_variable (cadr expression) state))))

(define operand2
  (lambda (expression state)
    (car (get_state_variable (caddr expression) state))))



; Retrieves the next statement of parse tree
(define statement ;(statement '((return 150) (return 120))) => (return 150)
  (lambda (parselist)
    (car parselist)))
    
; Retrieves the expression in a return statement
(define return_exp
  (lambda (parselist)
    (cadar parselist)))


 
; OKAY BELOW - Can recomment LATER!!!!

;This function takes a variable and finds the value associated with it on the state.

(define get_state_variable
  (lambda (n state) ; n is a variable
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
      ((eq? (caar state) var)   (cons (car state) (list (append (list n) (cdadr state)))) ) ;if it's the equal then change the associated value
      (else (append_two_lists (cons (list (caar state)) (list (list (caadr state)))) (set_state_variable var n (cons (cdar state) (list (cdadr state))))
      ))
      )))
;the last statement puts the first variable from state and it's value on the stack and later on appends it the new state.
;I create my own append function because it has to append things like ((v) (10)) ( (a b c) (10 11 12) ) for example.

(define append_two_lists
  (lambda (lis1 lis2)
    (cons (append (car lis1) (car lis2)) (list (append (cadr lis1) (cadr lis2))))))


; Not implemented: error checking on type
; Not implemented: short-circuit evaluation of && or ||
