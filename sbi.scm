#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
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

;
; File Retrieval Functions
;
(define *stderr* (current-error-port))

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

;(usage-exit)
;kills program and prints error message
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

; foldr 
(define (foldr f u l)
    (if (null? l) u
        (f (car l) (foldr f u (cdr l)))))

; filter
; filters a list based on a predicate
(define (filter p? l)
    (define (c a d) (if (p? a) (cons a d) d))
    (foldr c '() l))

;(readlist-from-inputfile)
;Input : (string filename) name of file to be read
;Output: list of strings containing individual 
;        lines in the input file
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         (filter (lambda (x) (not (null? (cdr x)))) program)))))

;(write-program-by-line filename program)
;Input  : (string filename) and (list program) which is 
;         a list of strings containing individual lines
;         in the input file
;Effects: Prints each line in the file to stdout
;Notes  : Used for debugging
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))
;
; Symbol Table Initialization & Functions
;

;TODO: define build-statement-table, build-label-table
;Create Tables
(define *statement-table* (make-hash))
(define *label-table* (make-hash))
(define *function-table* (make-hash))
(define *variable-table* (make-hash))

;General Functions
;
(define (symbol-get table key)
        (hash-ref table key))
(define (symbol-put! table key value)
        (hash-set! table key value))

(define (populate-table table pairs)
    (for-each (lambda (pair) (symbol-put! table (car pair) (cdr pair))) pairs))

(define (get-address line)
    (car line)
    )
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

;Statement Table

(define (build-statement-table program) 
    (populate-table *statement-table* 
        (map (lambda (line) 
            (list (get-address line) (get-statement line)) ) program)))

;Label Table
(define (build-label-table program) 
    (populate-table *label-table* 
        (map (lambda (line) 
            (list (get-label line) (get-address line)) ) program)))

;Function Table

;Variable Table

; Code for printing a hash table
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)
(define (print-hash-table ht) 
    (hash-for-each ht (lambda (key value) (show key value)))
    (newline))

; Interpreter
(define (interpreter line_list)
;    (map (lambda (x) (printf "~a~n" x)) line_list)
;				(printf "~n")
;    (map (lambda (x) (printf "~a~n" (cdr x))) line_list)
;				(printf "~n")
;    (map (lambda (x) (printf "~a~n" (cadr x))) line_list)
;				(printf "~n")
;    (map (lambda (x) 
;        (if (list? (cadr x)) (printf "~a~n" (cadr x)) 
;            (printf "~n")))
;        line_list)
;				(printf "~n")

    (define (interpret line)
;        (display (car line))
;        (display (cadar line))
;        (display (cddar line))
        (cond ((and (symbol? (cadar line)) (not (null? (cddar line))))
            (printf "~a~n" (car (cdr (caddar line))) ))
            (else (printf "")))
 ;       (printf "~n")
        (cond ( (list? (cadar line)) 
            (cond ((string=? "print" (symbol->string (caadar line))) 
                   (printf "~a~n" (car (cdadar line)))
                   (cond ( (null? (cdr line)) '()) ; at end of list?
                         ( else (interpret (cdr line))) ))
                  (else (cond ( (null? (cdr line)) '()) ; at end of list?
                        ( else (interpret (cdr line)))))))
            (else (cond ( (null? (cdr line)) '()) ; at end of list?
                         ( else (interpret (cdr line))) )) )
    )

;        (cond ((string=? "print" (symbol->string (caadar line))) 
;               (printf "~a~n" (car (cdadar line)))
;               (cond ( (null? (cdr line)) '()) ; at end of list?
;                     ( else (interpret (cdr line)))))
;		      ( else 
;				(display (car line))
;				(display " | ")
;				(display (cdar line))
;				(display " | ")
;				(display (cadar line))
;				(display " | ")
;				(display (symbol? (cadar line)))
;				(display " | ")
;				(display (caadar line))
;				(display " | ")
;				(printf "~n")
;				(cond ( (null? (cdr line)) '()) ; at end of list?
;				      ( else (interpret (cdr line)))) )))
	(interpret line_list))

;
; F. Main
;

;(main arglist)
(define (main arglist)
    ; if the arglist is null or has more than one element
    (if (or (null? arglist) (not (null? (cdr arglist)))) (usage-exit) ; check input
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (build-statement-table program) 
              (build-label-table program)
;              (display program)
;              (printf "~n")
 ;             (display (cdr (car program)))
              (interpreter program)
              ))); 

(main (vector->list (current-command-line-arguments)))

;(print-hash-table *statement-table*)
;(print-hash-table *label-table*)
;
;Error Checking Code
;

;(display (get-address '(1) )) (display "\n")
;(display (get-address '(1 "label") )) (display "\n")
;(display (get-address '(1 ("statement")) )) (display "\n")
;(display (get-address '(1 "label" ("statement")) )) (display "\n")
;
;(display (get-statement '(1) )) (display "\n")
;(display (get-statement '(1 "label") )) (display "\n")
;(display (get-statement '(1 ("statement")) )) (display "\n")
;(display (get-statement '(1 "label" ("statement")) )) (display "\n")

