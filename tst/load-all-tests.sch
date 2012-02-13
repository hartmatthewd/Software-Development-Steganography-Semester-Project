(load "tst/encoder/lsb.sch")
(load "tst/decoder/lsb.sch")

(define (run-all-tests)
   (run-lsb-encoding-tests)
   (run-lsb-decoding-tests))
