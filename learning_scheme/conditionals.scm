#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; if
; (if predicate then
;     else)
(define (sign_if n)
    (if (< n 0) -1
        (if (> n 0) +1
        0)))

(printf "~a~n" (sign_if -200))
(printf "~a~n" (sign_if 200))

; cond
; (cond ((predicate) then)
;   ((else-predicate) then)
;   (else then)) ; else here is literal string "else"
;
; the above if can be rewritten as
(define (sign_cond n)
    (cond ((< n 0) -1)
          ((> n 0) +1)
          (else 0)))

; Does a cond need to have an else?
(printf "~a~n" (cond ((< 0 1) 1)))

(printf "~a~n" (sign_cond -200))
(printf "~a~n" (sign_cond 200))
