#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; AUTHOR
;;    Name: Vincent Steffens
;;    ID: 1388341
;;    Email: vsteffen@ucsc.edu
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;; TO-DO
;;    * define build-statement-table, build-label-table
;;    * Consider eliminating symbol-get and symbol-put!. This requires
;;      at least rewriting populate-table in terms of hash-set!.
;;    * Reorganize functions
;;    * Describe and comment everything I've written
;;    * Put my name in this
;;    * Simplify the predicates in the first cond of interpret
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Tables
(define *statement-table* (make-hash))
(define *label-table* (make-hash))
;(define *function-table* (make-hash))
;(define *variable-table* (make-hash))

; Stderr
(define *stderr* (current-error-port))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

; foldr
; description
(define (foldr f u l)
    (if (null? l) u
        (f (car l) (foldr f u (cdr l)))))

; filter
; filters a list based on a predicate
(define (filter p? l)
    (define (c a d) (if (p? a) (cons a d) d))
    (foldr c '() l))

; readlist-from-inputfile
; Takes a filename and returns a list of the file's nonblank lines.
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         (filter(lambda (x)
                           (not (null? (cdr x)))) program)))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;(define (symbol-get table key)
;        (hash-ref table key))
;
;(define (symbol-put! table key value)
;        (hash-set! table key value))

; It may be worth redefining this in terms of hash-set!
(define (populate-table table pairs)
    (for-each (lambda (pair) (symbol-put! table (car pair) (cdr pair))) pairs))

; This may be unnecessary
(define (get-address line)
    (car line)
    )

; get-statement
; description
(define (get-statement line)
    (if (null? (cdr line)) ; line --> (number)
        '()
        (if (list? (cadr line)) ;line --> (number (statement))
            (cadr line)
            (if (null? (cddr line)) ;line --> (number label)
                '()
                (caddr line)) ;line --> (line_num label (statement))
            )
        )
    )

; get-label
; description
(define (get-label line)
    (if (null? (cdr line)) ; line --> (number)
        '()
        (if (list? (cadr line)) ;line --> (number (statement))
            '()
            (if (null? (cadr line)) ;line --> (number label)
                (cadr line)
                (cadr line)) ;line --> (line_num label (statement))
            )
        )
    )

; build-statement-table
; description
(define (build-statement-table program)
    (populate-table *statement-table*
        (map (lambda (line)
            (list (get-address line) (get-statement line)) ) program)))

; build-label-table
; description
(define (build-label-table program)
    (populate-table *label-table*
        (map (lambda (line)
            (list (get-label line) (get-address line)) ) program)))

; This can probably be turned into a lambda inside print-hash-table
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)

; Combine this function with show
(define (print-hash-table ht)
    (hash-for-each ht (lambda (key value) (show key value)))
    (newline))

; Function table
; Initialized when the program begins, using a for-each loop
; containing a lambda.
; See:
; * hashexample.scm, lines 21, 65
; * symbols.scm
(define *function-table* (make-hash))

(for-each
    (lambda (pair)
            (hash-set! *function-table* (car pair) (cadr pair)))
    `(

        (+ ,(lambda (x y) (+ x y)))
        (- ,-);(lambda (x y) (- x y)))
        (* ,*)
        (/ ,(lambda (x y)
                    (cond ((= 0 y)
                           (cond ((> x 0) "+inf.0")
                                 ((< x 0) "-inf.0")
                                 (else    "+nan.0")))
                          (else (/ x (+ y 0.0))) ) ))
        (abs ,abs)
        (acos ,acos)
        (asin ,asin)
        (atan ,atan)
        (cos ,cos)
        (floor ,floor)
        (log2 ,(lambda (x) (/ (log x) (log 2))))
        (round ,round)
        (sin ,sin)
        (sqrt ,sqrt)
        (tan ,tan)
        (trunc ,truncate)

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x)
                    (cond ((= x 0) "+nan.0")
                          (else (/ (log x) (log 10.0)))) ))

        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,(lambda (x)
                    (cond ((= x 0) "+nan.0")
                          (else (log x)) ) ))

        (sqrt    ,sqrt)

     ))

; Expression evaluator
(define (evaluate-expression expr)
   (cond ((number? expr)
;          (printf "number:     ~a~n" expr)
            expr)
;         ((null? (hash-ref *variable-table* expr #f))
;          (hash-ref *variable-table* expr))
         ((symbol? expr)
;          (printf "symbol:     ~a~n" expr)
          ; It's either a function
          (cond ((hash-ref *function-table* expr #f)
                 (hash-ref *function-table* expr #f))
                ((hash-ref *variable-table* expr #f)
                 (hash-ref *variable-table* expr #f))))
;          (print "~a~n" (hash-ref *function-table* expr #f))
;            (hash-ref *function-table* expr #f))
         ((pair? expr)
;          (printf "pair:       ~a~n" expr)
 ;         (printf "car expr:   ~a~n" (car expr))
  ;        (printf "cadr expr:   ~a~n" (cadr expr))
          ; it's either a function or a variable
          (cond ((hash-ref *function-table* (car expr) #f)
                 (apply (hash-ref *function-table* (car expr))
                        (map evaluate-expression (cdr expr)) ))
                ((hash-ref *variable-table* (car expr) #f)
                 (vector-ref 
                    (hash-ref *variable-table* (car expr)) 
                    (evaluate-expression (cadr expr)) ) )) )
        
         (else #f))
)

; build-variable-table
; description
; implementation
(define *variable-table* (make-hash))

(define (symbol-get table key)
        (hash-ref table key))

(define (symbol-put! table key value)
        (hash-set! table key value))

; Functions for dealing with each type of statement
(define (statement-dim arguments)
;    (printf "Hello from dim!~n")
;    (printf "~a~n" arguments)
;    (printf "~a~n" (car arguments))
;    (printf "~a~n" (caar arguments))
;    (printf "~a~n" (cdar arguments))
;    (printf "~a~n" (evaluate-expression (cadar arguments)))
    (hash-set! *variable-table* (caar arguments) 
        (make-vector (evaluate-expression (cadar arguments))))
)

(define (statement-let arguments)
;    (printf "Hello from let!~n")
 ;   (printf "~a~n" arguments)
  ;  (printf "~a~n" (car arguments))
   ; (printf "~a~n" (cadr arguments))
    ;(printf "~a~n" (list? (car arguments)))
    (cond 
        ((list? (car arguments)) ; list of array and index
    ;        (printf "~a~n" (caar arguments))
     ;       (printf "~a~n" (cadar arguments))
      ;      (printf "~a~n" (hash-ref *variable-table* (cadar arguments)))

       ;  (printf "~a~n" (hash-ref *variable-table* (caar arguments)))
         (vector-set! 
            (hash-ref *variable-table* (caar arguments))
            (evaluate-expression (hash-ref *variable-table* (cadar arguments)))
            (cadr arguments)
         )
        ; (printf "~a~n" (hash-ref *variable-table* (caar arguments)))
         ;(printf "caar: ~a~n" (caar arguments))   
        )
        (else 
;            (printf "else~n")
 ;           (printf "~a~n" (car arguments))
  ;          (printf "~a~n" (cadr arguments))
   ;         (printf "~a~n" (evaluate-expression (cadr arguments)))
            (hash-set! 
                *variable-table* 
                (car arguments) 
                (evaluate-expression (cadr arguments)))
        )
    )
;    (define memory (car arguments))
;    (printf "~a~n" (cdr arguments))
;    (printf "~a~n" (cadr arguments))
;    (define expression (evaluate-expression (cadr arguments)))
;    (printf "~a~n" (evaluate-expression (cadr arguments)))
    
)

(define (statement-goto arguments)
    (printf "Hello from goto!~n")
    (printf "~a~n" arguments)
)

(define (statement-if arguments)
    (printf "hello from if!~n")
    (printf "~a~n" arguments)
)

; Test esp. with 22-fibonacci
(define (statement-print arguments)
;    (printf "~nprint: ~n")
;    (printf "* arguments                 = ~a.~n" arguments)
;    (printf "* (null? arguments)         = ~a.~n" (null? arguments))

    (cond
        ((null? arguments) (printf "~n"))
        (else
;            (printf "* (car arguments)           = ~a.~n" (car arguments))
;            (printf "* (string? (car arguments)) = ~a.~n" (string? (car arguments)))
            (cond
                ; If we're printing a string
                ((string? (car arguments))
                    ; print the string.
                    (printf (car arguments))
                    ; then print the rest
                    (statement-print (cdr arguments))
                )
                ; If we are instead printing an expression
                (else
                    ; print the expression
;                    (printf "~a~n" (car arguments))
                    (printf "~a" (evaluate-expression (car arguments)))
                    ; then print the rest
                    (statement-print (cdr arguments))
                )
            )
        )
    )
)

(define (statement-input arguments)
    (printf "Hello from input!~n")
    (printf "~a~n" arguments)
)

(define (interpret-statement statement)
    (cond
        ((string=? "dim" (symbol->string (car statement)))
            (statement-dim (cdr statement)))

        ((string=? "let" (symbol->string (car statement)))
            (statement-let (cdr statement)))

        ((string=? "goto" (symbol->string (car statement)))
            (statement-goto (cdr statement)))

        ((string=? "if" (symbol->string (car statement)))
            (statement-if (cdr statement)))

        ((string=? "print" (symbol->string (car statement)))
            (statement-print (cdr statement)))

        ((string=? "input" (symbol->string (car statement)))
            (statement-input (cdr statement)))
    )
)

; Interpreter
; TO-DO
; * Comment this
; * Put the print conditions in one statement
; * Put the combined print code in its own function, and call that
;   function from here.
(define (interpreter line_list)
    (define (interpret line)

        ; The line is the whole list
        ;(printf "line: ~a~n" line)
        ; The car of the line is the line
        ;(printf "car line:                ~a~n" (car line))
        ; The cdr of the car of the line is the rest of the line after
        ; the first element. This is a list.
        ;(printf "cdar line:               ~a~n" (cdar line))
        ; The car of the cdar is the line is the line's second element
        ;(printf "cadar line:              ~a~n" (cadar line))
        ; If this is a symbol, then it's a label
        ;(printf "(symbol? (cadar line)):  ~a~n" (symbol? (cadar line)))
        ; If there's a label, then the rest might either be
        ; * a statement, or
        ; * nothing
        ;(printf "cddar line:              ~a~n" (cddar line))
        ; So...
        (cond
            ; ((predicate)
            ; if the line has a label and the rest of the line isn't null...
            ((and (symbol? (cadar line)) (not (null? (cddar line))))
            ;  (then))
            ; pass (caddar line) on to interpret-statement
            (interpret-statement (caddar line)))

            ; ((predicate)
            ; if the line has a label, but the rest is null...
            ((and (symbol? (cadar line)) (null? (cddar line)))
            ; (then))
            ; do nothing
            (printf ""))

            ; and if it doesn't have a label...
            ; ( predicate
            ( else
            ;   (then))
            ; pass (cadar line) on to interpret-statement
            (interpret-statement (cadar line)) )
        )

        (cond ((null? (cdr line)) '())          ; at end of list?
              (else (interpret (cdr line)))
        )

            ; See if it's a print statement
;            (printf "print command?:          ~a~n"
;                (string=? "print" (symbol->string (car (caddar line)))))
;            (cond
;                ; ((predicate1)
;                ((string=? "print" (symbol->string (car (caddar line)))) ; ((predicate)
;                ; (then1))
;                (printf "print command:           ~a~n" (car (caddar line)))
;                (printf "rest of line:            ~a~n~n" (car (cdr (caddar line)))))
;                ; ((predicate2)
;                ((string=? "dim" (symbol->string (car (caddar line))))
;                ;  (then2))
;                 (printf "Array declared~n"))
;
;            ))
;              (else
;            (printf "Not a print statement.~n~n")))

;        (cond
;            ; Does this line have a label?
;            ; Is the rest of the line not blank?
;            ( (and (symbol? (cadar line)) (not (null? (cddar line)))) ; ( (predicate)
;                (printf "~a~n" (car (cdr (caddar line)))) )           ;   (then) )
;                (else (printf ""))
;        ) ; cond

;        (cond
;            ; ((predicate1)
;            ((list? (cadar line))
;            ;  (then1))
;            ;(printf "Predicate 1 is true~n")
;            (cond
;                ; ((predicate1a)
;                ((string=? "print" (symbol->string (caadar line)))
;                ;  (then))
;                   (printf "FOO: ~a~n" (car (cdadar line)))
;                   (cond ( (null? (cdr line)) '()) ; at end of list?
;                         ( else (interpret (cdr line))) ))
;                ; (else
;                  (else (printf "Predicate 1a is false~n")
;                        (cond ((null? (cdr line)) '()) ; at end of list?
;                              (else (interpret (cdr line))) ))
;            ))
;
;            ; ( else (then))
;            (else (printf "Predicate 1 is false~n")
;                  (cond ((null? (cdr line)) '())          ; at end of list?
;                        (else (interpret (cdr line))) )
;            )
;        )
    )
    (interpret line_list))

; Main
(define (main arglist)
    ; if the arglist is null or has more than one element
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((inputfile (car arglist))
               (program (readlist-from-inputfile inputfile)))
              (build-statement-table program)
              (build-label-table program)
              (interpreter program)
              )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Execution Begins Here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main (vector->list (current-command-line-arguments)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(print-hash-table *statement-table*)
;(print-hash-table *label-table*)

;(display (get-address '(1) )) (display "\n")
;(display (get-address '(1 "label") )) (display "\n")
;(display (get-address '(1 ("statement")) )) (display "\n")
;(display (get-address '(1 "label" ("statement")) )) (display "\n")
;
;(display (get-statement '(1) )) (display "\n")
;(display (get-statement '(1 "label") )) (display "\n")
;(display (get-statement '(1 ("statement")) )) (display "\n")
;(display (get-statement '(1 "label" ("statement")) )) (display "\n")
