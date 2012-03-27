(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/util.rkt")
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
    (write-bytestring-to-file (decode-payload (make-decoder carrier)) output))

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

(define (decode-next-byte decoder)
    (let [(bits (make-vector 8))
          (decode-func (lambda (frequencies i) (get-bit-from-frequency (vector-ref frequencies i))))]
         (vector-map! (lambda (b) (code-next-frequency decoder decode-func)) bits)
         (get-byte-from-bit-vector bits)))

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
(define (is-same-angle? a b)
    (or (< (abs (- a b)) round-off-error)
        (< (abs (- b a)) round-off-error)))

;;;;;;;;;;;;;;;;;;
(define (angle-is-one? x)
    (or (is-same-angle? x 0)
        (is-same-angle? x pi/2)
        (is-same-angle? x pi)
        (is-same-angle? x 3pi/2)))
