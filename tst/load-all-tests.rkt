(require rackunit)

(define (reset-for-next-test)
   (load "src/requirements.rkt")
   (load "src/constants.rkt"))

(define (run-all-tests)
   (reset-for-next-test)
   (load "tst/util.rkt")
   (run-util-tests)

   (reset-for-next-test)
   (load "tst/fileio.rkt")
   (run-fileio-tests)

   (reset-for-next-test)
   (load "tst/phase-coder.rkt")
   (run-phase-coder-tests)

   (reset-for-next-test)
   (load "tst/encoder.rkt")
   (run-encoder-tests)

   (reset-for-next-test)
   (load "tst/decoder.rkt")
   (run-decoder-tests)
)
