#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
(define (sign n)
	(if (< n 0) -1 
	(if (> n 0) +1
	0)))

(sign -200)
(sign 200)
