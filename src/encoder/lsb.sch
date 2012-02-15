; WARNING - current version has no protection from the payload being too large to fit in the carrier.

; Returns a byte equal to the given byte only with the least significant bit set to the given bit
(define (encode-bit-into-byte-lsb byte bit)
   (bitwise-ior bit (bitwise-and byte (bitwise-not 1))))

; Given a bytevector, an index of a byte to change, and a bit, change the lsb of byte at that index to the given bit
(define (encode-bit-into-carrier-lsb carrier cindex bit)
   (bytevector-set! 
      carrier
      cindex
      (encode-bit-into-byte-lsb (bytevector-ref carrier cindex) bit)))

; Given a carrier and a starting index, write the given byte into the carrier via lsb encoding
; writing 1 bit of the payload every 2 bytes of the carrier
(define (encode-byte-into-carrier-lsb carrier cindex byte)
   (let ((bits (get-bits-from-byte byte)))
     (letrec
       ((encode-bit (lambda (cindex bit-index)
         (if (< bit-index 8)
             (begin (encode-bit-into-carrier-lsb carrier cindex (vector-ref bits bit-index))
                    (encode-bit (+ cindex 2) (+ bit-index 1)))
             cindex))))
       (encode-bit cindex 0))))
     
; Given a carrier and a starting index, write the given payload into the carrier via lsb encoding 
; writing 1 bit of the payload every 2 bytes of the carrier
(define (encode-payload-via-lsb carrier cindex payload)
   (letrec
     ((encode-byte (lambda (cindex pindex)
       (if (< pindex (bytevector-length payload))
           (encode-byte (encode-byte-into-carrier-lsb carrier cindex (bytevector-ref payload pindex)) (+ pindex 1))
           cindex))))
     (encode-byte cindex 0)))
