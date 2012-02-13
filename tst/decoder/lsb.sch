(load "src/decoder/lsb.sch")

(define (run-lsb-decoding-tests)
   (display "Running lsb decoding tests") (newline)
   (get-lsb-test))

(define (get-lsb-test)
  (assert (= (get-lsb 7) 1))
  (assert (= (get-lsb 8) 0)))
