(define (run-encoder-tests)
   (load "src/encoder.rkt")

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
   (display "No test cases for encode-payload-into-carrier\n"))

(define (encode-payload-size-test)
   ;(encode-payload-size payload encoder)
   (display "No test cases for encode-payload-size\n"))

(define (encode-bytes-test)
   ;(encode-bytes payload encoder)
   (display "No test cases for encode-bytes\n"))

(define (encode-byte-test)
   ;(encode-byte byte encoder)
   (display "No test cases for encode-byte\n"))

(define (encode-bit-test)
   ;(encode-bit bit encoder)
   (display "No test cases for encode-bit\n"))

(define (get-shifted-frequency-test)
   ;(get-shifted-frequency frequency bit)
   (check-equal? (get-shifted-frequency 123.456+123i 0) 123.22821092590772+123.22821092590769i)
   (check-equal? (get-shifted-frequency 6345.5345+534.345i 0) 4502.8509119065475+4502.850911906547i)
   (check-equal? (get-shifted-frequency 123.456+123i 1) 174.2710071583911+0.0i)
   (check-equal? (get-shifted-frequency 6345.5345+534.345i 1) 6367.992828962298+0.0i))

(define (get-bits-from-byte-test)
   ;(get-bits-from-byte byte)
   (let [(v8 (get-bits-from-byte 8)) 
         (v13 (get-bits-from-byte 13))]
        (check-equal? (vector-ref v8 4) 1)
        (check-equal? (vector-ref v13 4) 1)
        (check-equal? (vector-ref v13 5) 1)
        (check-equal? (vector-ref v13 7) 1)))

(define (encode-bit-into-frequency-test)
   (let [(t (vector 0 1 2 3 4 3 2 1))]
        (encode-bit-into-frequency t 0 1)
        (encode-bit-into-frequency t 1 2)
        (check-equal? t 
                      (vector 0 
                              (get-shifted-frequency 1 0) 
                              (get-shifted-frequency 2 1) 
                              3 4 3 2 1))))
