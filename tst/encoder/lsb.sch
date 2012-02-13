(load "src/encoder/lsb.sch")

(define (run-lsb-encoding-tests)
    (display "Running lsb encoding tests") (newline)
    (encode-lsb-tests))

(define (encode-lsb-tests)
   (assert (= (encode-lsb 57 0) 56))
   (assert (= (encode-lsb 4 0) 4))
   (assert (= (encode-lsb -1232 0) -1232))
   (assert (= (encode-lsb -2343 0) -2344))

   (assert (= (encode-lsb 57 1) 57))
   (assert (= (encode-lsb 4 1) 5))
   (assert (= (encode-lsb -1232 1) -1231))
   (assert (= (encode-lsb -2343 1) -2343)))
