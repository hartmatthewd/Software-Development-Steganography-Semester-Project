; Given a byte, return the least significant bit
(define (get-lsb byte)
   (bitwise-and byte 1))
