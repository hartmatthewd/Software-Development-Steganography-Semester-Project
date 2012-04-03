(define (run-encoder-decoder-tests)
    (load "src/encoder.rkt")
    (load "src/decoder.rkt")

    (encode-payload-into-carrier testwav testpayload tmptemp)
    (decode-payload-from-carrier tmptemp tmptemp)
    (payloads-equal? testpayload tmptemp)

    (encode-payload-into-carrier testmp3 testpayload tmptemp)
    (decode-payload-from-carrier tmptemp tmptemp)
    (payloads-equal? testpayload tmptemp))

(define (payloads-equal? p1 p2)
   (let [(b1 (file->bytes p1)) (b2 (file->bytes p2)) (diff 0)]
        (for [(i (bytes-length b1))]
             (let [(bits1 (get-bits-from-byte (bytes-ref b1 i)))
                   (bits2 (get-bits-from-byte (bytes-ref b2 i)))]
                  (for [(j 8)]
                       (when (not (= (vector-ref bits1 j) (vector-ref bits2 j)))
                             (set! diff (add1 diff))))))
        (check-true (< (/ diff (* 8 (bytes-length b1))) biterrorlimit))))
