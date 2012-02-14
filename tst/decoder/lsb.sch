(load "src/decoder/lsb.sch")

(define (run-lsb-decoding-tests)
   (display "tst/decoder/lsb.sch") (newline)
   (get-lsb-test)
   (get-lsb-from-bytevector-test))

(define (get-lsb-test)
  (assert (= (get-lsb 7) 1))
  (assert (= (get-lsb 8) 0)))

(define (get-lsb-from-bytevector-test)
  (let ((bv (make-bytevector 2 0)))
     (bytevector-s8-set! bv 1 1)
     (assert (= (get-lsb-from-bytevector bv 0) 0))
     (assert (= (get-lsb-from-bytevector bv 1) 1))))
