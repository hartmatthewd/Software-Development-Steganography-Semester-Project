(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/frequencycoder.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Decoder
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given carrier and stick it in the given output file
;;; carrier - the carrier where to find the hidden message
;;; output - the file to output the hidden message into
(define (decode-payload-from-carrier carrier output)
    (bytes->file (decode-payload (make-decoder carrier)) output))

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given coder and return it as a bytestring
;;; decoder - the coder where to find the hidden message

(define (decode-payload decoder)
    (let [(bytes (make-bytes (decode-payload-size decoder)))]
         (for [(i (bytes-length bytes))]
             (bytes-set! bytes i (decode-next-byte decoder)))
         bytes))

;;;;;;;;;;;;;;;;;;
;;; Given a coder, decode the payload size
;;; decoder - the coder where to find the hidden message

(define (decode-payload-size decoder)
    (validate-payload-size decoder 
                           (integer-bytes->integer (bytes (decode-next-byte decoder) 
                                                          (decode-next-byte decoder)
                                                          (decode-next-byte decoder)
                                                          (decode-next-byte decoder)) 
                                                   #f 
                                                   'little)))

;;;;;;;;;;;;;;;;;;
;;; Given a coder, decode the payload
;;; decoder - the coder where to find the hidden message

(define (decode-next-byte decoder)
    (let [(bits (make-vector 8))]
         (vector-map! (lambda (b) (code-next-frequency decoder decode-next-bit)) bits)
         (get-byte-from-bit-vector bits)))


;;;;;;;;;;;;;;;;;
(define (decode-next-bit frequencies indexes)
    (let [(bits (make-vector (vector-length indexes)))]
         (for [(i (vector-length indexes))]
              (vector-set! bits i (get-bit-from-frequency (vector-ref frequencies (vector-ref indexes i)))))
         (let [(zero 0) (one 0)]
              (vector-map (lambda (x) (if (= 1 x) (set! one (add1 one)) (set! zero (add1 zero)))) bits)
              (if (> one zero) 1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a frequency, return what bit is encoded in it
;;; frequency - the frequency to pull an encoded bit out of

(define (get-bit-from-frequency frequency)
    (if (angle-is-one? (abs (angle frequency))) 1 0))

;;; The amount of error we allow for round off error in determining the phase of a frequency
;;; on decoding
(define round-off-error (/ pi 8))

;;;;;;;;;;;;;;;;;;
;;; Tests if the two given angles are the same within a round off error limit
;;; a - an angle to test
;;; b - an angle to test

(define (is-same-angle? a b)
    (or (< (abs (- a b)) round-off-error)
        (< (abs (- b a)) round-off-error)))

;;;;;;;;;;;;;;;;;;
;;; Returns true if the given angle represents a binary one
;;; x - the angle to test

(define (angle-is-one? x)
    (or (is-same-angle? x 0)
        (is-same-angle? x pi/2)
        (is-same-angle? x pi)
        (is-same-angle? x 3pi/2)))

;;;;;;;;;;;;;;;;;;
;;; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
;;; bits - the vector of bits of which to make a byte out of

(define (get-byte-from-bit-vector bits)
  (do [(index 1 (add1 index))
       (byte (vector-ref bits 0) (bitwise-ior (vector-ref bits index) (arithmetic-shift byte 1)))]
      [(= index 8) byte]))
