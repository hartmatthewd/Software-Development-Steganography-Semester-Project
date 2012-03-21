(load "src/phase-coder.rkt")

(define (run-phase-coder-tests)
   (display "tst/phase-coder.rkt") (newline)
   (get-shift-test)
   (get-shifted-frequency-test)
   (get-bit-from-frequency-test))

(define (get-shift-test)
    (check-equal? (get-shift 0) zero-shift)
    (check-equal? (get-shift 1) one-shift))

(define (get-shifted-frequency-test)
    (check-equal? (angle (get-shifted-frequency 123.456+123i 0)) zero-shift)
    (check-equal? (angle (get-shifted-frequency 6345.5345+534.345i 0)) zero-shift)
    (check-equal? (angle (get-shifted-frequency 123.456+123i 1)) one-shift)
    (check-equal? (angle (get-shifted-frequency 6345.5345+534.345i 1)) one-shift))

(define (get-bit-from-frequency-test)
    (check-equal? (get-bit-from-frequency (get-shifted-frequency 123.456+123i 0)) 0)
    (check-equal? (get-bit-from-frequency (get-shifted-frequency 6345.5345+534.345i 0)) 0)
    (check-equal? (get-bit-from-frequency (get-shifted-frequency 123.456+123i 1)) 1)
    (check-equal? (get-bit-from-frequency (get-shifted-frequency 6345.5345+534.345i 1)) 1))
