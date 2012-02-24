(load "tst/encoder/util.sch")
(load "tst/decoder/util.sch")
(load "tst/shared/util.sch")

(define (run-all-tests)
   (run-encoder-util-tests)
   (run-decoder-util-tests)
   (run-util-tests)

   (display "--------------------")
   (newline)
   (display "All tests succeeded!"))
