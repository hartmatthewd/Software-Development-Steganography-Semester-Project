; Given a byte, return a list whos elements are the individual bits of the byte
(define (get-bits-from-byte byte)
  (let ((v (make-vector 8)))
    (letrec 
      ((set-bit (lambda (index byte)
        (if (>= index 0) 
          (begin (vector-set! v index (bitwise-and byte 1)) 
                 (set-bit (- index 1) (bitwise-arithmetic-shift-right byte 1))))))) 
      (set-bit 7 byte)) v))

; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
(define (get-byte-from-bit-vector bitvector)
  (letrec
    ((set-bit (lambda (index byte)
      (if (= index 8)
          byte
          (set-bit (+ index 1) (bitwise-ior (vector-ref bitvector index) (bitwise-arithmetic-shift-left byte 1)))))))
    (set-bit 1 (bitwise-ior 0 (vector-ref bitvector 0)))))
