; Returns a byte equal to the given byte only with the least significant bit set to the given bit
(define (encode-lsb byte bit)
   (bitwise-ior bit (bitwise-and byte (bitwise-not 1))))

; Returns a byte equal to the given byte only with the most significant bit set to the given bit
(define (encode-msb byte bit)
   (let ((byte (bitwise-and byte (greatest-fixnum))))
     (if (= bit 1)
         (bitwise-ior byte (least-fixnum)) 
         byte)))
