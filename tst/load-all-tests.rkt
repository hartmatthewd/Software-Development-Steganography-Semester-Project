(require rackunit)

(load "src/encoder.rkt")
(load "src/decoder.rkt")

;; 1 second of stereo audio, all samples are 0
(define (create-test-wavfile)
    (create-wavfile #f 'little 1 2 44100 176400 4 16 44 176444))

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

   (load "tst/wavfile.rkt")
   (run-wavfile-tests)
)
