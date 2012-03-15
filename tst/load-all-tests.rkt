(require rackunit)
(load "tst/util.rkt")
(load "tst/fileio.rkt")

(define (run-all-tests)
   (run-util-tests)
   (run-fileio-tests))
