;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; encoder.rkt
;
; Entry level file for encoding a payload into a wave
; or mp3 file.
;
; To encode, users should use the method:
;
;     (encode-payload-into-carrier carrier payload output)
;
;         carrier - file path of the wave or mp3 file to encode
;                   the payload into
;         payload - file path of the payload to encode into the
;                   the carrier
;         output  - file path of where to output the carrier
;                   after the payload has been encoded
;
; No other public methods are required for encoding
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/frequencycontroller.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Encoder
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a payload, a carrier file, and an output file, encode the payload into the carrier and write it 
;     to the output file
; inputs
;     payload (string?) - path to the payload to encode
;     carrier (string?) - path to the .wav or .mp3 file to encode the payload into
;     output (string?) - the path of the file which to output the encoded .wav or .mp3 to
; outputs
;     void

(define (encode-payload-into-carrier carrier payload output)
    (let* [(payload-bytes (file->bytes payload))
           (controller (make-encoder carrier output (bytes-length payload-bytes)))]
          (encode-payload-size payload-bytes controller)
          (encode-bytes payload-bytes controller)
          (finalize-controller controller)))

;;;;;;;;;;;;;;;;;;
; Given a bytestring payload and a controller, encode the payload size into the controller
; inputs
;     payload (bytes?) - the bytes of a payload whose size to encode
;     controller (controller?) - the controller of which to encode the payload size into
; outputs
;     void

(define (encode-payload-size payload controller)
    (encode-bytes (integer->integer-bytes (bytes-length payload) 4 #f 'little) controller))

;;;;;;;;;;;;;;;;;;
; Given a bytestring payload and a controller, encode the payload into the controller
; inputs
;     payload (bytes?) - the bytes of a payload
;     controller (controller?) - the controller of which to encode the payload into
; outputs
;     void

(define (encode-bytes payload controller)
    (for [(p (bytes-length payload))]
         (encode-byte (bytes-ref payload p) controller)))

;;;;;;;;;;;;;;;;;;
; Given a byte and a controller encode the byte into the controller at some frequency
; inputs
;     byte (byte?) - the byte to encode
;     controller (controller?) - the controller to encode the byte into
; outputs
;     void

(define (encode-byte byte controller)
    (vector-map (lambda (b) (encode-bit b controller)) (get-bits-from-byte byte)))

;;;;;;;;;;;;;;;;;;
; Given a bit to encode, and a controller, encode the give bit into the controller
; inputs
;     bit (real? -> 1 or 0) - the bit to encode
;     controller (controller?) - the controller to encode the bit into
; outputs
;     void

(define (encode-bit bit controller)
    (code-next-frequency controller 
                         (lambda (frequencies indexes)
                                 (vector-map (lambda (i)
                                                     (encode-bit-into-frequency frequencies 
                                                                                bit 
                                                                                i 
                                                                                pi/4)
                                                     (encode-bit-into-frequency frequencies 
                                                                                bit 
                                                                                (- (vector-length frequencies) i)
                                                                                -pi/4))
                                             indexes))))

;;;;;;;;;;;;;;;;;;
; Given a vector or frequencies in the frequency domain, a bit to encode, and an index of a frequency in the vector,
;     encode the bit into the frequency at the given index of the frequencies vector
; inputs
;     frequencies (vector?) - the vector of frequencies
;     bit (real? -> 1 or 0) - the bit to encode
;     i (real?) - the index of the frequencies to encode the given bit into 
; outputs
;     void

(define (encode-bit-into-frequency frequencies bit i default-boost-angle)
    (maybe-boost-frequency frequencies i default-boost-angle)
    (vector-set! frequencies i (get-shifted-frequency (vector-ref frequencies i) bit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Encoding Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Returns a frequency representing the given frequency with the given bit encoded into it
; inputs
;     frequency (complex?) - the frequency of which to encode the given bit into
;     bit (real? -> 1 or 0) - the bit to encode
; outputs
;     complex?

(define (get-shifted-frequency frequency bit)
    (if (= bit 0)
        (make-polar (magnitude frequency) (+ (* pi/2 (round (- (/ (angle frequency) pi/2) 0.5))) (/ pi/2 2)))
        (make-polar (magnitude frequency) (* pi/2 (round (/ (angle frequency) pi/2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
; Maybe boost the magnitude of the frequency at index i in the frequencies vector if it is outside a set range
; inputs
;     frequencies (vector?) - a vector of frequencies
;     i (real?) - an index of the frequencies vector corresponding to which frequency to possibly boost
;     default-boost-angle (real?) - if the frequency to boost in currently 0, the default angle to set it to when 
;                                   it is boosted
; outputs
;     void

(define (maybe-boost-frequency frequencies i default-boost-angle)
    (let [(freq (vector-ref frequencies i))]
         (when (< (magnitude freq) min-magnitude)
               (let [(ang (if (= freq 0)
                              default-boost-angle
                              (angle freq)))]
                    (vector-set! frequencies i (make-polar min-magnitude ang))))))


;;;;;;;;;;;;;;;;;;
; Given a byte, return a vector whos elements are the individual bits of the byte
; inputs
;     byte (real?) - the byte of which to return a vector of its bits
; outputs
;     vector?

(define (get-bits-from-byte byte)
  (let ((v (make-vector 8)))
    (do [(i 7 (sub1 i)) (b byte (arithmetic-shift b -1))]
        [(< i 0)]
        (vector-set! v i (bitwise-and b 1)))
    v))
