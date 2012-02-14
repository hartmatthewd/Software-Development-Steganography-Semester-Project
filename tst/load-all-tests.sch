(load "tst/encoder/lsb.sch")
(load "tst/decoder/lsb.sch")
(load "tst/shared/util.sch")

(define (run-all-tests)
   (run-lsb-encoding-tests)
   (run-lsb-decoding-tests)
   (run-util-tests))
