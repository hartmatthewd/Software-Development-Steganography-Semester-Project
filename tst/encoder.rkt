(define (run-encoder-tests)
   (display "tst/encoder.rkt") (newline)
   (encode-payload-into-carrier-test)
   (encode-payload-size-test)
   (encode-bytes-test)
   (encode-byte-test)
   (encode-bit-test)
   (get-shifted-frequency-test)
   (get-bits-from-byte-test)
   (encode-bit-into-frequency-test))

(define (encode-payload-into-carrier-test)
;(encode-payload-into-carrier carrier payload output)
(error "No test cases for encode-payload-into-carrier"))

(define (encode-payload-size-test)
;(encode-payload-size payload encoder)
(error "No test cases for encode-payload-size"))

(define (encode-bytes-test)
;(encode-bytes payload encoder)
(error "No test cases for encode-bytes"))

(define (encode-byte-test)
;(encode-byte byte encoder)
(error "No test cases for encode-byte"))

(define (encode-bit-test)
;(encode-bit bit encoder)
(error "No test cases for encode-bit"))

(define (get-shifted-frequency-test)
;(get-shifted-frequency frequency bit)
    (display "WARNING: Update this when we fix the phase shift")(newline)
    (check-equal? (angle (get-shifted-frequency 123.456+123i 0)) (inexact zero-shift))
    (check-equal? (angle (get-shifted-frequency 6345.5345+534.345i 0)) (inexact zero-shift))
    (check-equal? (angle (get-shifted-frequency 123.456+123i 1)) (inexact one-shift))
    (check-equal? (angle (get-shifted-frequency 6345.5345+534.345i 1)) (inexact one-shift)))

(define (get-bits-from-byte-test)
;(get-bits-from-byte byte)
   (let ((v8 (get-bits-from-byte 8)) (v13 (get-bits-from-byte 13)))
     (check-equal? (vector-ref v8 4) 1)
     (check-equal? (vector-ref v13 4) 1)
     (check-equal? (vector-ref v13 5) 1)
     (check-equal? (vector-ref v13 7) 1)))

(define (encode-bit-into-frequency-test)
;(encode-bit-into-frequency frequencies bit i)
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
