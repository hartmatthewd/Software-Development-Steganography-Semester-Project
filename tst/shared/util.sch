(load "src/shared/util.sch")

(define (run-util-tests)
   (display "tst/shared/util.sch") (newline)
   (get-bits-from-byte-test))

(define (get-bits-from-byte-test)
   (let ((v8 (get-bits-from-byte 8)) (v13 (get-bits-from-byte 13)))
     (assert (= (vector-ref v8 4) 1))
     (assert (= (vector-ref v13 4) 1))
     (assert (= (vector-ref v13 5) 1))
     (assert (= (vector-ref v13 7) 1))))
