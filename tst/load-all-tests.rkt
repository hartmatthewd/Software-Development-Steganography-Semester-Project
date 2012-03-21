(require rackunit)

(define (run-all-tests)
   (load "tst/util.rkt")
   (run-util-tests)

   (load "tst/fileio.rkt")
   (run-fileio-tests)

   (load "tst/phase-coder.rkt")
   (run-phase-coder-tests)

   (load "tst/encoder.rkt")
   (run-encoder-tests)

   (load "tst/decoder.rkt")
   (run-decoder-tests)
)
