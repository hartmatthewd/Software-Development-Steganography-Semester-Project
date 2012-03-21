(load "src/fileio.rkt")
(load "src/util.rkt")
(load "src/phase-coder.rkt")

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given carrier and stick it in the given output file
;;; carrier - the carrier where to find the hidden message
;;; output - the file to output the hidden message into

(define (decode-payload-from-carrier carrier output)
    (write-bytestring-to-file (decode-payload-from-wav (file->wavfile carrier)) output))

;;;;;;;;;;;;;;;;;;
;;; THIS WILL NEED TO BE GENERATED DYNAMICALLY SOMEHOW
(define num-of-bytes-to-decode 4)

;;;;;;;;;;;;;;;;;;
;;; decode a secret message from the given wav and return it as a bytestring
;;; wav - the wavfile where to find the hidden message

(define (decode-payload-from-wav wav)
    (let [(bytes (make-bytes num-of-bytes-to-decode))
          (bits (make-vector 8))]
         (do [(i 0 (add1 i))]
             [(= i (bytes-length bytes))]
             (vector-map! (lambda (b) (decode-next-bit wav)) bits)
             (bytes-set! bytes i (get-byte-from-bit-vector bits)))
         bytes))

;;;;;;;;;;;;;;;;;;
;;; decode and return the next bit from the given wavfile
;;; wav - the wavfile where to find the next bit

(define (decode-next-bit wav)
    (let* [(samples (vector-ref (wavfile-samples wav) 0))
           (i (get-next-sample-index))
           (frequencies (fft (vector-copy samples i (+ i samples-per-fft))))]
          (get-bit-from-frequency (vector-ref frequencies (get-fundamental-frequency frequencies)))))
