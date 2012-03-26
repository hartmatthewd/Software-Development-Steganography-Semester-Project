(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/wavfile.rkt")
(load "src/fileio.rkt")
(load "src/util.rkt")
(load "src/phase-coder.rkt")

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given carrier and stick it in the given output file
;;; carrier - the carrier where to find the hidden message
;;; output - the file to output the hidden message into

(define (decode-payload-from-carrier carrier output)
    (write-bytestring-to-file (decode-payload-from-wav (file->wavfile-decoder carrier)) output))

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given wav and return it as a bytestring
;;; wav - the wavfile where to find the hidden message

(define (decode-payload-from-wav wav)
    (let [(bytes (make-bytes (decode-payload-size-from-wavfile wav)))]
         (for [(i (bytes-length bytes))]
             (bytes-set! bytes i (decode-next-byte wav)))
         bytes))

;;;;;;;;;;;;;;;;;;
;;; Given a wavfile, decode the payload size

(define (decode-payload-size-from-wavfile wav)
    (validate-payload-size wav (integer-bytes->integer (bytes (decode-next-byte wav) 
                                                              (decode-next-byte wav)
                                                              (decode-next-byte wav)
                                                              (decode-next-byte wav)) 
                                                       #f 
                                                       'little)))

;;;;;;;;;;;;;;;;;;
;;; wav the wavfile where to decode the next byte from

(define (decode-next-byte wav)
    (let [(bits (make-vector 8))]
         (vector-map! (lambda (b) (decode-next-bit wav)) bits)
         (get-byte-from-bit-vector bits)))

;;;;;;;;;;;;;;;;;;
;;; decode and return the next bit from the given wavfile channel
;;; wav - the wavfile where to find the next bit
;;; channel - the index of a channel in the wavfile

(define (decode-next-bit wav)
    (let [(frequencies (fft (get-next-samples wav)))]
         (get-bit-from-frequency (vector-ref frequencies frequency-to-encode))))
