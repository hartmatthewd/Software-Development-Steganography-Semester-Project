(load "src/shared/fileio.sch")
(load "src/decoder/util.sch")

; Given a byte, return the least significant bit
(define (get-lsb byte)
   (bitwise-and byte 1))

; Given a bytevector and an index within the bytevector, return the value of the lsb of the byte at the given index
(define (get-lsb-from-bytevector bytevector index)
   (get-lsb (bytevector-ref bytevector index)))

; Given a carrier and a starting index, return another bytevector whos bits are all the lsb of every second byte of the carrier
(define (get-payload-from-carrier-lsb carrier cindex)
   ; init the payload
   (let ((payload (make-bytevector (ceiling (/ (- (bytevector-length carrier) cindex) 16)) 0)))
     (letrec
       ((fill-bitvector (lambda (cindex pindex bitvector bindex)
         ; if the current bitvector is full, turn it into a byte and add it to the payload, then recurse
         (if (= bindex 8)
             (begin (bytevector-set! payload pindex (get-byte-from-bit-vector bitvector))
                    (fill-bitvector cindex (+ pindex 1) (make-vector 8 0) 0))
             ; if the carrier pointer is beyond the end of the carrier, add what we have to the payload and return the payload
             (if (<= (bytevector-length carrier) cindex)
                 (begin (if (> bindex 0)
                            (bytevector-set! payload pindex (get-byte-from-bit-vector bitvector)))
                        payload)
                 ; else set the next bit and recurse
                 (begin (vector-set! bitvector bindex (get-lsb-from-bytevector carrier cindex))
                        (fill-bitvector (+ cindex 2) pindex bitvector (+ bindex 1))))))))
       (fill-bitvector cindex 0 (make-vector 8 0) 0))))

; Perform decoding operation given string filenames for carrier and out
(define (decode-lsb carrier out)
   (write-bytevector-to-file out 
      (get-payload-from-carrier-lsb (read-file-into-bytevector carrier) 44)))
