(define (run-encoder-decoder-tests)
    (vector-map (lambda (x)
                        (display (string-append "Testing: " x))(newline)
                        (encode-payload-into-carrier x testpayload tmptemp)
                        (decode-payload-from-carrier tmptemp tmptemp)
                        (payloads-equal? testpayload tmptemp))
                (vector testwav testwav-2 testmp3 testmp3-2 testmp3-3)))

(define (payloads-equal? p1 p2)
   (let [(b1 (file->bytes p1)) (b2 (file->bytes p2)) (diff 0)]
        (for [(i (min (bytes-length b1) (bytes-length b2)))]
             (let [(bits1 (get-bits-from-byte (bytes-ref b1 i)))
                   (bits2 (get-bits-from-byte (bytes-ref b2 i)))]
                  (for [(j 8)]
                       (when (not (= (vector-ref bits1 j) (vector-ref bits2 j)))
                             (set! diff (add1 diff))))))
        (when (or (not (= (bytes-length b1) (bytes-length b2)))
                  (> (/ diff (* 8 (bytes-length b1))) biterrorlimit))
              (fail (string-append "BER outside allowable range -- payload length: "
                                   (number->string (bytes-length b1))
                                   " -- decoded payload length: "
                                   (number->string (bytes-length b2))
                                   " -- BER: "
                                   (number->string (inexact (/ diff (* 8 (bytes-length b1))))))))))
