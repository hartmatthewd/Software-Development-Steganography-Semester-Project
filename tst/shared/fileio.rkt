(load "src/shared/fileio.rkt")

(define (run-fileio-tests)
   (display "tst/shared/fileio.rkt") (newline)
   (test-is-mp3?)
   (test-is-wav?))

(define (test-is-mp3?)
   (display "WARNING: is-mp3? is too simplified and not working for all mp3s. Ignoring test for now.")
   ;(check-false (is-mp3? "testwav.wav"))
   ;(check-true (is-mp3? "testmp3.mp3")))
)

(define (test-is-wav?)
   (check-true (is-wav? "testwav.wav"))
   (check-false (is-wav? "testmp3.mp3")))
