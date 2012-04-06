(define (run-encoder-tests)
   (load "src/encoder.rkt")

   (get-shifted-frequency-test)
   (get-bits-from-byte-test)
   (encode-bit-into-frequency-test))

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
   (let [(t (vector 0 1 2 3 4 3 2 1))
         (y (vector 0 (get-shifted-frequency 1 0) (get-shifted-frequency 2 1) 3 4 3 2 1))]
        (encode-bit-into-frequency t 0 1 pi/4)
        (encode-bit-into-frequency t 1 2 pi/4)
        (maybe-boost-frequency y 1 pi/4)
        (maybe-boost-frequency y 2 pi/4)
        (check-equal? t y)))
