(require rackunit)
(load "tst/encoder/util.rkt")
(load "tst/decoder/util.rkt")
(load "tst/shared/util.rkt")

(define (run-all-tests)
   (run-encoder-util-tests)
   (run-decoder-util-tests)
   (run-util-tests)

   (display "--------------------")
   (newline)
   (display "All tests succeeded!"))
