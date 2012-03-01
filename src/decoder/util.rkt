; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
(define (get-byte-from-bit-vector bits)
  (letrec
    ((set-bit (lambda (index byte)
      (if (= index 8)
          byte
          (set-bit (+ index 1) (bitwise-ior (vector-ref bits index) (arithmetic-shift byte 1)))))))
    (set-bit 1 (bitwise-ior 0 (vector-ref bits 0)))))
