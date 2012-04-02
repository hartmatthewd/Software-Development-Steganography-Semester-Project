(require rackunit)

(define (run-all-tests)
   (load "src/requirements.rkt")
   (load "src/constants.rkt")

   (load "tst/fileio.rkt")
   (run-fileio-tests)

   (load "tst/wavfile.rkt")
   (run-wavfile-tests)

   (load "tst/frequencycoder.rkt")
   (run-frequencycoder-tests)

   (load "tst/encoder.rkt")
   (run-encoder-tests)

   (load "tst/decoder.rkt")
   (run-decoder-tests)
)
