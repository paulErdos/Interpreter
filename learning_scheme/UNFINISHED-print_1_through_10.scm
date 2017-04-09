#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

; count the length of a list
; accumulator-style tail call
; from Mackey's notes
(define len (lambda (l)
	    (define len.. (lambda (l.. n)
	    		  (if (null? l..) n               ; if l.. is null, return n
			      (len.. (cdr l..) (+ n 1))))) ; else, call len.. with cdr of l.. and add one to n
	    (len.. l 0)))
		
; accumulate from 1 to n, and print out each number
(define count (lambda (n) 
	      (define count.. (lambda (n k)
			      (printf "~a~n" k)
		              (if (not (< k n)) 
				  k
				  (count.. n (+ k 1)) )))
	      (count.. n 0)))

(count 10)
