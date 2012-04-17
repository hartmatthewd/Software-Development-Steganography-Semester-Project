;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; decoder.rkt
;
; Entry level file for decoding a payload out of a wave
; or mp3 file.
;
; To decode, users should use the method:
;
;     (decode-payload-from-carrier carrier output)
;
;         carrier - file path of a wave or mp3 file containing
;                   an encoded message
;         output  - file path of where to write the decoded
;                   message
;
; No other public methods are required for decoding
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-relative "requirements.rkt")
(load-relative "constants.rkt")
(load-relative "settings.rkt")
(load-relative "frequencycontroller.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Decoder
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; decode a secret message from the given carrier and stick it in the given output file
; inputs
;     carrier (string?) - path to the carrier where to find the hidden message
;     output (string?) - path to where to output the hidden message
; outputs
;     void

(define (decode-payload-from-carrier carrier output)
    (bytes->file (decode-payload (make-decoder carrier)) output))

;;;;;;;;;;;;;;;;;;
; decode a secret message from the given controller and return it as a bytestring
; inputs
;     controller (controller?) - the controller where to find the hidden message
; outputs
;     bytes?

(define (decode-payload controller)
    (let [(bytes (make-bytes (decode-payload-size controller)))]
         (for [(i (bytes-length bytes))]
             (bytes-set! bytes i (decode-next-byte controller)))
         bytes))

;;;;;;;;;;;;;;;;;;
; Given a conroller, decode the payload size
; inputs
;     controller (controller?) - the controller where to find the hidden message
; outputs
;     real?

(define (decode-payload-size controller)
    (validate-payload-size controller 
                           (integer-bytes->integer (bytes (decode-next-byte controller) 
                                                          (decode-next-byte controller)
                                                          (decode-next-byte controller)
                                                          (decode-next-byte controller)) 
                                                   #f 
                                                   'little)))

;;;;;;;;;;;;;;;;;;
; Given a controller, decode the payload
; inputs
;     controller (controller?) - the controller where to find the hidden message
; outputs
;     byte?

(define (decode-next-byte controller)
    (let [(bits (make-vector 8))]
         (vector-map! (lambda (b) (code-next-frequency controller decode-next-bit)) bits)
         (get-byte-from-bit-vector bits)))


;;;;;;;;;;;;;;;;;
; Decode the next bit from the given frequencies vector
; inputs
;     frequencies (vector?) - a vector of frequencies to where to decode the next bit from
;     indexes (vector?) - a vector of indexes corresponding to the frequencies vector of where to decode the next bit.
;                         See frequency-components-to-encode in constants.rkt for more info
; outputs
;     real? (1 or 0)

(define (decode-next-bit frequencies indexes)
    (let [(zero 0) (one 0)]
         (for [(i (vector-length indexes))]
							(if (= 1 (get-bit-from-frequency (vector-ref frequencies (vector-ref indexes i))))
							    (set! one (add1 one)) 
								  (set! zero (add1 zero))))
         ; Ties go to 1 to ensure full message is always decoded
         (if (>= one zero) 1 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Decoding Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a frequency, return what bit is encoded within it
; inputs
;     frequency (complex?) - the frequency to pull an encoded bit out of
; outputs
;     real? (1 or 0)

(define (get-bit-from-frequency frequency)
    (if (angle-is-one? (abs (angle frequency))) 1 0))

;;;;;;;;;;;;;;;;;;
; The amount of error we allow for round off error in determining the phase of a frequency
;     on decoding
(define round-off-error (/ pi 8))

;;;;;;;;;;;;;;;;;;
; Tests if the two given angles are the same within a round off error limit
; inputs
;     a (real?) - an angle to test
;     b (real?) - an angle to test
; outputs
;     boolean?

(define (is-same-angle? a b)
    (or (< (abs (- a b)) round-off-error)
        (< (abs (- b a)) round-off-error)))

;;;;;;;;;;;;;;;;;;
; Returns true if the given angle represents a binary one
; inputs
;     x (real?) - the angle to test
; outputs
;     boolean?

(define (angle-is-one? x)
    (let [(a (abs x))]
         (or (is-same-angle? a 0)
             (is-same-angle? a pi/2)
             (is-same-angle? a pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a vector of bits of length 8, returns a byte whos bits match those in the vector
; inputs
;     bits (vector?) - the vector of bits of which to make a byte out of
; outputs
;     byte?

(define (get-byte-from-bit-vector bits)
  (do [(index 1 (add1 index))
       (byte (vector-ref bits 0) (bitwise-ior (vector-ref bits index) (arithmetic-shift byte 1)))]
      [(= index 8) byte]))
