(define samples-per-fft 1024)
(define duplicate-encoding-each-channel #f)
(define shift (/ pi 2))


;;; Given a payload, a carrier file, and an output file, encode the payload into the carrier and write
;;; it to the output file

(define (encode-payload-into-carrier payload carrier output)
    (let [(payload-bytes (read-file-into-bytestring payload))
          (wav (file->wavfile carrier))]
         (do [(p 0 (+ p 1))]
             [(= p (bytes-length payload-bytes))]
             (encode-byte-into-wavfile (bytes-ref payload-bytes p) wav))
         (wavfile->file wav output)))


;;; Given a byte and a wavfile, encode the byte into the wavfile samples

(define (encode-byte-into-wavfile byte wav)
    (let [(bits (get-bits-from-byte byte))]
         (do [(i 0 (+ i 1))]
             [(= i (vector-length bits))]
             (encode-bit-into-wavfile (vector-ref bits i) wav))))


;;; Given a bit and a wavfile, encode the bit into the wavfile

(define (encode-bit-into-wavfile bit wav)
    ;;; For now encodes only in first channel
    (let* [(samples (vector-ref (wavfile-samples wav) 0))
           (i (get-next-sample-index samples))
           (frequencies (fft (vector-copy samples i (+ i samples-per-fft))))]
          ;;; some junk here
          (vector-copy! samples i (sanitize-samples (fft-inverse frequencies)))))


;;; Sanatize the frequency vactor to ensure that each sample is an exact integer
;;; (round off error in the fft can make them slightly off)

(define (sanitize-samples frequencies)
    (do [(i 0 (+ i 1))]
        [(= i (vector-length frequencies)) frequencies]
        (vector-set! frequencies i (exact (round (real-part (vector-ref frequencies i)))))))


;;; Returns the next index of the samples to encode to

(define (get-next-sample-index samples)
    (modulo (exact (floor (* (random) (vector-length samples)))) samples-per-fft))


;;; Given a complex number (representing a sample in the frequency domain) and a bit to encode,
;;; return a new sample with the bit encoded

(define (get-shifted-value orig bit)
    (make-polar (magnitude orig) (get-shift bit)))


;;; Given a bit, return the amount to shift the phase

(define (get-shift bit)
    (if (= bit 0)
        -shift
        shift))
