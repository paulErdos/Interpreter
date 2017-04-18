#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; CONTENTS
; topic  .. line
; lambda .. 7
; map    .. 20
; let    .. 44

; lambda
; r5rs       page 9, 16, 41
; t-y-scheme page 15
; (lambda (argument_list) (expr ...))
; A lambda expression evaluates to a procedure
(printf "~a~n" (number->string ((lambda (x) (+ x x)) 4)))

(define poly1 (lambda (x) (+ (* 3 (* x x x)) 5)))
(printf "~a~n" (poly1 5))


; map
; r5rs       page 32
; t-y-scheme page 27
; (map proc list1 list2 ...)
; 
; Applies a function to one or more equally-sized lists, and returns a list
(printf "~a~n" (map cadr '((a b) (c d) (e f))))

; I'm having trouble figuring out where lambdas are actually necessary
(define (scalar_product x1 x2)
	(map * x1 x2))
(printf "~a~n" (scalar_product '(1 0 1) '(1 1 0)))

; Let's make a scalar product that takes two lists
; converts them to unit lists
; and returns the scalar product
(define (unit_scalar_product x1 x2)
    (apply + 
        (map * 
            (map (lambda (x) (/ x (apply + x1))) x1)
	    (map (lambda (x) (/ x (apply + x2))) x2))))
(printf "~a~n" (unit_scalar_product '(1 0 1) '(1 1 0)))


; let
; r5rs page 11, 12, 15, 16, 43
; t-y-scheme page 22, 26
; (let <bindings> <body>)
; (let ((var1 init1) ...) expr ...)
;      ^                ^
;      |                |
;      this             and this
;
; Note these parens enclosing the list of initialized variables
;
; The inits are eval'd, the vars are bound to fresh locations holding the
; results, the body is eval'd, and the value of the last expression in the
; body is returned. 
;
; A let statement is also syntactic sugar for a lambda followed by its 
; arguments. The arguments are eval'd and passed to the lambda, which is 
; then eval'd
;
; so this:
;(define (angle_between_vectors x1 x2)
;    (define (unit v) (map (lambda (vi) (/ vi (apply + v))) v))
;    (acos (apply + (map * (unit x1) (unit x2)))))
;(printf "~a~n" (angle_between_vectors '(1 0 1) '(1 1 0)))
; is the same as this:
(define (angle_between_vectors x1 x2)
    (let ((unit (lambda (v) (map (lambda (vi) (/ vi (apply + v))) v) ))) 
         (acos (apply + (map * (unit x1) (unit x2)) )) ))
(printf "~a~n" (angle_between_vectors '(1 0 1) '(1 1 0)))

; and this
(printf "~a~n" (let ((x 2) (y 3)) (+ x y)))
; is the same as this
(printf "~a~n" ((lambda (x y) (+ x y)) 2 3))
