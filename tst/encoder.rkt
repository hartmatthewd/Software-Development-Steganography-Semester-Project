(define (run-encoder-tests)
   (display "tst/encoder.rkt") (newline)
   (sanitize-samples-test)
   (encode-bit-into-frequency-test)
)

(define (encode-bit-into-frequency-test)
    (let [(t (vector 0 1 2 3 4 5 5 4 3 2 1))]
         (encode-bit-into-frequency t 0 1)
         (encode-bit-into-frequency t 1 2)
         (check-equal? t 
                       (vector 0 
                               (get-shifted-frequency 1 0) 
                               (get-shifted-frequency 2 1) 
                               3 4 5 5 4 3 
                               (get-shifted-frequency 2 1) 
                               (get-shifted-frequency 1 0)))))

(define (sanitize-samples-test)
   (check-equal? (sanitize-samples (vector 1 2 3 4 5))
                 (vector 1 2 3 4 5))
   (check-equal? (sanitize-samples (vector 153.99+5e-10i 50000 -50000 -343.342+234.342i 5))
                 (vector 154 32767 -32768 -343 5)))
