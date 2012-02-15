(load "src/encoder/lsb.sch")

(define (run-lsb-encoding-tests)
    (display "tst/encoding/lsb.sch") (newline)
    (encode-bit-into-byte-lsb-tests)
    (encode-bit-into-carrier-lsb-tests)
    (encode-byte-into-carrier-lsb-tests)
    (encode-payload-via-lsb-test))

(define (encode-bit-into-byte-lsb-tests)
   (assert (= (encode-bit-into-byte-lsb 57 0) 56))
   (assert (= (encode-bit-into-byte-lsb 4 0) 4))
   (assert (= (encode-bit-into-byte-lsb -1232 0) -1232))
   (assert (= (encode-bit-into-byte-lsb -2343 0) -2344))

   (assert (= (encode-bit-into-byte-lsb 57 1) 57))
   (assert (= (encode-bit-into-byte-lsb 4 1) 5))
   (assert (= (encode-bit-into-byte-lsb -1232 1) -1231))
   (assert (= (encode-bit-into-byte-lsb -2343 1) -2343)))

(define (encode-bit-into-carrier-lsb-tests)
   (let ((bv (make-bytevector 4 0)))
      (bytevector-set! bv 0 5)
      (bytevector-set! bv 1 5)
      (bytevector-set! bv 2 8)
      (bytevector-set! bv 3 8)
      (encode-bit-into-carrier-lsb bv 0 1)
      (encode-bit-into-carrier-lsb bv 1 0)
      (encode-bit-into-carrier-lsb bv 2 1)
      (encode-bit-into-carrier-lsb bv 3 0)
      (assert (= (bytevector-ref bv 0) 5))
      (assert (= (bytevector-ref bv 1) 4))
      (assert (= (bytevector-ref bv 2) 9))
      (assert (= (bytevector-ref bv 3) 8))))

(define (encode-byte-into-carrier-lsb-tests)
    (let ((bv (make-bytevector 16 5)))
      (encode-byte-into-carrier-lsb bv 0 6)
      (assert (= (bytevector-ref bv 0) 4))
      (assert (= (bytevector-ref bv 1) 5))
      (assert (= (bytevector-ref bv 2) 4))
      (assert (= (bytevector-ref bv 3) 5))
      (assert (= (bytevector-ref bv 4) 4))
      (assert (= (bytevector-ref bv 5) 5))
      (assert (= (bytevector-ref bv 6) 4))
      (assert (= (bytevector-ref bv 7) 5))
      (assert (= (bytevector-ref bv 8) 4))
      (assert (= (bytevector-ref bv 9) 5))
      (assert (= (bytevector-ref bv 10) 5))
      (assert (= (bytevector-ref bv 11) 5))
      (assert (= (bytevector-ref bv 12) 5))
      (assert (= (bytevector-ref bv 13) 5))
      (assert (= (bytevector-ref bv 14) 4))
      (assert (= (bytevector-ref bv 15) 5)))

    (let ((bv (make-bytevector 16 4)))
      (encode-byte-into-carrier-lsb bv 0 6)
      (assert (= (bytevector-ref bv 0) 4))
      (assert (= (bytevector-ref bv 1) 4))
      (assert (= (bytevector-ref bv 2) 4))
      (assert (= (bytevector-ref bv 3) 4))
      (assert (= (bytevector-ref bv 4) 4))
      (assert (= (bytevector-ref bv 5) 4))
      (assert (= (bytevector-ref bv 6) 4))
      (assert (= (bytevector-ref bv 7) 4))
      (assert (= (bytevector-ref bv 8) 4))
      (assert (= (bytevector-ref bv 9) 4))
      (assert (= (bytevector-ref bv 10) 5))
      (assert (= (bytevector-ref bv 11) 4))
      (assert (= (bytevector-ref bv 12) 5))
      (assert (= (bytevector-ref bv 13) 4))
      (assert (= (bytevector-ref bv 14) 4))
      (assert (= (bytevector-ref bv 15) 4))))

(define (encode-payload-via-lsb-test)
   (let ((carrier (make-bytevector 32 5)) (payload (make-bytevector 2 6)))
     (encode-payload-via-lsb carrier 0 payload)
     (assert (= (bytevector-ref carrier 0) 4))
     (assert (= (bytevector-ref carrier 1) 5))
     (assert (= (bytevector-ref carrier 2) 4))
     (assert (= (bytevector-ref carrier 3) 5))
     (assert (= (bytevector-ref carrier 4) 4))
     (assert (= (bytevector-ref carrier 5) 5))
     (assert (= (bytevector-ref carrier 6) 4))
     (assert (= (bytevector-ref carrier 7) 5))
     (assert (= (bytevector-ref carrier 8) 4))
     (assert (= (bytevector-ref carrier 9) 5))
     (assert (= (bytevector-ref carrier 10) 5))
     (assert (= (bytevector-ref carrier 11) 5))
     (assert (= (bytevector-ref carrier 12) 5))
     (assert (= (bytevector-ref carrier 13) 5))
     (assert (= (bytevector-ref carrier 14) 4))
     (assert (= (bytevector-ref carrier 15) 5))
     (assert (= (bytevector-ref carrier 16) 4))
     (assert (= (bytevector-ref carrier 17) 5))
     (assert (= (bytevector-ref carrier 18) 4))
     (assert (= (bytevector-ref carrier 19) 5))
     (assert (= (bytevector-ref carrier 20) 4))
     (assert (= (bytevector-ref carrier 21) 5))
     (assert (= (bytevector-ref carrier 22) 4))
     (assert (= (bytevector-ref carrier 23) 5))
     (assert (= (bytevector-ref carrier 24) 4))
     (assert (= (bytevector-ref carrier 25) 5))
     (assert (= (bytevector-ref carrier 26) 5))
     (assert (= (bytevector-ref carrier 27) 5))
     (assert (= (bytevector-ref carrier 28) 5))
     (assert (= (bytevector-ref carrier 29) 5))
     (assert (= (bytevector-ref carrier 30) 4))
     (assert (= (bytevector-ref carrier 31) 5)))

   (let ((carrier (make-bytevector 32 4)) (payload (make-bytevector 2 6)))
     (encode-payload-via-lsb carrier 0 payload)
     (assert (= (bytevector-ref carrier 0) 4))
     (assert (= (bytevector-ref carrier 1) 4))
     (assert (= (bytevector-ref carrier 2) 4))
     (assert (= (bytevector-ref carrier 3) 4))
     (assert (= (bytevector-ref carrier 4) 4))
     (assert (= (bytevector-ref carrier 5) 4))
     (assert (= (bytevector-ref carrier 6) 4))
     (assert (= (bytevector-ref carrier 7) 4))
     (assert (= (bytevector-ref carrier 8) 4))
     (assert (= (bytevector-ref carrier 9) 4))
     (assert (= (bytevector-ref carrier 10) 5))
     (assert (= (bytevector-ref carrier 11) 4))
     (assert (= (bytevector-ref carrier 12) 5))
     (assert (= (bytevector-ref carrier 13) 4))
     (assert (= (bytevector-ref carrier 14) 4))
     (assert (= (bytevector-ref carrier 15) 4))
     (assert (= (bytevector-ref carrier 16) 4))
     (assert (= (bytevector-ref carrier 17) 4))
     (assert (= (bytevector-ref carrier 18) 4))
     (assert (= (bytevector-ref carrier 19) 4))
     (assert (= (bytevector-ref carrier 20) 4))
     (assert (= (bytevector-ref carrier 21) 4))
     (assert (= (bytevector-ref carrier 22) 4))
     (assert (= (bytevector-ref carrier 23) 4))
     (assert (= (bytevector-ref carrier 24) 4))
     (assert (= (bytevector-ref carrier 25) 4))
     (assert (= (bytevector-ref carrier 26) 5))
     (assert (= (bytevector-ref carrier 27) 4))
     (assert (= (bytevector-ref carrier 28) 5))
     (assert (= (bytevector-ref carrier 29) 4))
     (assert (= (bytevector-ref carrier 30) 4))
     (assert (= (bytevector-ref carrier 31) 4))))
