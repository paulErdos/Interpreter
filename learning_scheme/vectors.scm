#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; declare an empty vector of size 5
; vectors are initialized to zero
(define x1 (make-vector 5))
(printf "x1: ~a~n" x1)

; declare an initialized vector
(define x2 (vector 0 1 2 3 4))
(printf "x2: ~a~n" x2)

; (map proc list1 list2 ...)
; vector->list
(printf "~a~n" (map + (vector->list x2) (vector->list x2)))

; vector-ref, vector-set!
(printf "x1[0]: ~a~n" (vector-ref x1 0))
(vector-set! x1 0 1)
(printf "x2[0]: ~a~n" (vector-ref x1 0))

((
