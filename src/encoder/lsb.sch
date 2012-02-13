; Returns a byte equal to the given byte only with the least significant bit set to the given bit
(define (encode-lsb byte bit)
   (bitwise-ior bit (bitwise-and byte (bitwise-not 1))))
