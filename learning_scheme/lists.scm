#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; for a list with several items
(define small_list '(1 2 3))
(not (null? (cdr small_list)))
; #t

; for a list with one item
(define one_item_list '(1))
(not (null? (cdr one_item_list)))
; #f
