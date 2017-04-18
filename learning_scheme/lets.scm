#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; A let form binds a series of identifiers, each to the result of some
; expression, for use in the let body

(let ((x 2) (y 3)) (+ x y))
; 5

; Let is also syntactic sugar for a lambda followed by its arguments which
; are first evaluated and then passed to the lambda which is then eval'd
; so this
(let ((a '(1 2 3)) (b '(3 4 6))) (cons a b))
; is the same as this
((lambda(a b) (cons a b)) '(1 2 3) '(4 5 6))

; and this
(printf "~a~n" (let ((x 2) (y 3)) (+ x y)))
; is the same as this
(printf "~a~n" ((lambda (x y) (+ x y)) 2 3))

; practicing from main program
;(define tiny_list '(1))
;(let* ((name (car tiny_list))
       
