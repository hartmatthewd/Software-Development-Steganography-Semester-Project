; Given a byte, return a list whos elements are the individual bits of the byte
(define (get-bits-from-byte byte)
  (let ((v (make-vector 8)))
    (letrec 
      ((set-bit (lambda (index byte)
        (if (>= index 0) 
          (begin (vector-set! v index (bitwise-and byte 1)) 
                 (set-bit (- index 1) (bitwise-arithmetic-shift-right byte 1))))))) 
      (set-bit 7 byte)) v))
