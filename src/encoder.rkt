(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/util.rkt")
(load "src/frequencycoder.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Encoder
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a payload, a carrier file, and an output file, encode the payload into the carrier and write
;;; it to the output file
;;; payload - the payload to encode
;;; carrier - the .wav or .mp3 file to encode the payload into
;;; output - the name of the file which to output the encoded .wav or .mp3 to

(define (encode-payload-into-carrier payload carrier output)
    (let [(payload-bytes (read-file-into-bytestring payload))
          (encoder (make-encoder carrier output))]
         (ensure-destination-large-enough? encoder (bytes-length payload-bytes))
         (encode-payload-size payload-bytes encoder)
         (encode-bytes payload-bytes encoder)
         (finalize-coder encoder)))

;;;;;;;;;;;;;;;;;;
;;; Given a payload and a coder, encode the payload size into the coder

(define (encode-payload-size payload encoder)
    (encode-bytes (integer->integer-bytes (bytes-length payload) 4 #f 'little) encoder))

;;;;;;;;;;;;;;;;;;
;;; Given a payload and a coder, encode the payload into the coder

(define (encode-bytes payload encoder)
    (for [(p (bytes-length payload))]
         (encode-byte (bytes-ref payload p) encoder)))

;;;;;;;;;;;;;;;;;;
;;; Given a byte and a coder encode the byte into the coder at some frequency
;;; byte - the byte to encode
;;; encoder - the coder to encode the byte into

(define (encode-byte byte encoder)
    (vector-map (lambda (b) (encode-bit b encoder)) (get-bits-from-byte byte)))

;;;;;;;;;;;;;;;;;;
;;; Given a bit to encode, and a coder, encode the give bit into the coder
(define (encode-bit bit encoder)
    (let [(encode-func (lambda (frequencies i)
                               (encode-bit-into-frequency frequencies bit i)
                               (encode-bit-into-frequency frequencies bit (- (vector-length frequencies) i))))]
         (code-next-frequency encoder encode-func)))

;;;;;;;;;;;;;;;;;;
;;; Given a vector or frequencies in the frequency domain, a bit to encode, and an index of a frequency in the vector,
;;; encode the bit into the frequency at the given index
;;; frequencies - the vector of frequencies
;;; bit - the bit to encode
;;; i - the index of the frequencies to encode the given bit into 

(define (encode-bit-into-frequency frequencies bit i)
    (vector-set! frequencies i (get-shifted-frequency (vector-ref frequencies i) bit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded
;;; frequency - the frequency of which to encode the given bit into
;;; bit - the bit to encode

(define (get-shifted-frequency frequency bit)
    (if (= bit 0)
        (make-polar (magnitude frequency) (+ (* pi/2 (round (- (/ (angle frequency) pi/2) 0.5))) (/ pi/2 2)))
        (make-polar (magnitude frequency) (* pi/2 (round (/ (angle frequency) pi/2))))))
