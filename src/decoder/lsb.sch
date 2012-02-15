; Given a byte, return the least significant bit
(define (get-lsb byte)
   (bitwise-and byte 1))

; Given a bytevector and an index within the bytevector, return the value of the lsb of the byte at the given index
(define (get-lsb-from-bytevector bytevector index)
   (get-lsb (bytevector-s8-ref bytevector index)))
