(load "src/shared/fileio.sch")
(load "src/encoder/util.sch")

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
  (ensure-able-to-encode-lsb carrier cindex payload)
  (letrec
    ((encode-byte (lambda (cindex pindex)
      (if (< pindex (bytevector-length payload))
          (encode-byte (encode-byte-into-carrier-lsb carrier cindex (bytevector-ref payload pindex)) (+ pindex 1))
          cindex))))
    (encode-byte cindex 0)))

; Ensure that the payload is small enough to fit into the carrier
(define (ensure-able-to-encode-lsb carrier cindex payload)
  (if (> (* (- (bytevector-length payload) 1) 16) (- (bytevector-length carrier) cindex))
      (error "Cannot encode data - payload is too large")))

; Perform encoding operation given string filenames for carrier, payload, and out
(define (encode-lsb carrier payload out) 
   (let ((carrier-bv (read-file-into-bytevector carrier)) (payload-bv (read-file-into-bytevector payload))) 
      (encode-payload-via-lsb carrier-bv 44 payload-bv) 
      (write-bytevector-to-file out carrier-bv)))
