(load "src/requirements.rkt")
(load "src/constants.rkt")
(load "src/fileio.rkt")
(load "src/util.rkt")
(load "src/phase-coder.rkt")

;;;;;;;;;;;;;;;;;;
;;; Given a payload, a carrier file, and an output file, encode the payload into the carrier and write
;;; it to the output file
;;; payload - the payload to encode
;;; carrier - the .wav or .mp3 file to encode the payload into
;;; output - the name of the file which to output the encoded .wav or .mp3 to

(define (encode-payload-into-carrier payload carrier output)
    (intialize)
    (let [(payload-bytes (read-file-into-bytestring payload))
          (wav (file->wavfile carrier))]
         (encode-byte-into-wavfile (bytes-length payload-bytes) wav)
         (for [(p (bytes-length payload-bytes))]
             (encode-byte-into-wavfile (bytes-ref payload-bytes p) wav))
         (wavfile->file wav output))
    (finalize))


;;;;;;;;;;;;;;;;;;
;;; Given a byte and a wavfile, encode the byte into the wavfile samples
;;; byte - the byte to encode
;;; wav - the wavfile to encode the byte into

(define (encode-byte-into-wavfile byte wav)
    (let [(bits (get-bits-from-byte byte))]
         (vector-map (lambda (b) (encode-bit-into-wavfile b wav)) bits)))

;;;;;;;;;;;;;;;;;;
;;; Given a bit and a wavfile, encode the bit into the wavfile
;;; bit - the bit to encode
;;; wav - the wavfile to encode the bit into
(define (encode-bit-into-wavfile bit wav)
    (encode-bit-into-wavfile-on-channel-at-sample bit wav 0 (get-next-sample-index)))

;;;;;;;;;;;;;;;;;;
;;; Given a bit to encode, a wavfile, a channel index in the wavfile, and a sample index
;;; in the channel, encode the give bit into the channel of the wavfile at the given sample
(define (encode-bit-into-wavfile-on-channel-at-sample bit wav channel sample)
    (let* [(samples (vector-ref (wavfile-samples wav) channel))
           (frequencies (fft (vector-copy samples sample (+ sample samples-per-fft))))]
          (encode-bit-into-frequency frequencies bit (get-fundamental-frequency frequencies))
          (vector-copy! samples sample (sanitize-samples (fft-inverse frequencies)))))

;;;;;;;;;;;;;;;;;;
;;; Given a vector or frequencies in the frequency domain, a bit to encode, and an index of a frequency in the vector,
;;; encode the bit into the frequency at the given index
;;; frequencies - the vector of frequencies
;;; bit - the bit to encode
;;; i - the index of the frequencies to encode the given bit into 

(define (encode-bit-into-frequency frequencies bit i)
    (let [(val (get-shifted-frequency (vector-ref frequencies i) bit))]
         (vector-set! frequencies i val)
         (vector-set! frequencies (- (vector-length frequencies) i) val)))


;;;;;;;;;;;;;;;;;;
;;; Sanatize the frequency vactor to ensure that each sample is an exact integer
;;; (round off error in the fft can make them slightly off)
;;; samples - the vector of samples to sanitize

(define (sanitize-samples samples)
    (vector-map! (lambda (x) (min 32767 (max -32768 (exact (round (real-part x)))))) samples))
