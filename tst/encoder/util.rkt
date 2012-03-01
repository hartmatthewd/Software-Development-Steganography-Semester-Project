(load "src/encoder/util.rkt")

(define (run-encoder-util-tests)
   (display "tst/encoder/util.rkt") (newline)
   (get-bits-from-byte-test))

(define (get-bits-from-byte-test)
   (let ((v8 (get-bits-from-byte 8)) (v13 (get-bits-from-byte 13)))
     (check-equal? (vector-ref v8 4) 1)
     (check-equal? (vector-ref v13 4) 1)
     (check-equal? (vector-ref v13 5) 1)
     (check-equal? (vector-ref v13 7) 1)))
