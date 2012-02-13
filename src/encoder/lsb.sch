; Returns a byte equal to the given byte only with the least significant bit set to the given bit
(define (encode-lsb byte bit)
   (bitwise-ior bit (bitwise-and byte (bitwise-not 1))))

; Given a bytevector, an index of a byte to change, and a bit, change the lsb of byte at that index to the given bit
(define (encode-lsb-in-bytevector bytevector index bit)
    (bytevector-s8-set! 
       bytevector 
       index 
       (encode-lsb (bytevector-s8-ref bytevector index) bit)))
