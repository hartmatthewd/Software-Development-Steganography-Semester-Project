;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; A holder for all the data needed to recreate the wav file
(struct wavfile (is-wav [encode-sample #:mutable] [encode-channel #:mutable]
                 endianess audioformat channels samplerate byterate
                 blockalign bytespersample chunkstart chunksize samples))

;;;;;;;;;;;;;;;;;;
;;; Common method to create a wavfile
(define (create-wavfile is-wav e af c sr by ba bps s cs)
    (let [(wav (wavfile is-wav -1 (- c 1) e af c sr by ba (/ bps 8) s (- cs s) (make-vector c)))]
         (intialize-wavfile-samples wav)
         wav))

;;;;;;;;;;;;;;;;;;
;;; Given a wavfile, intialize its samples vector to contain multiple vectors

(define (intialize-wavfile-samples wav)
    (let [(samples-per-channel (/ (wavfile-chunksize wav)
                                  (* (wavfile-bytespersample wav)
                                     (wavfile-channels wav))))]
    (vector-map! (lambda (s) (make-vector samples-per-channel)) (wavfile-samples wav))))

;;;;;;;;;;;;;;;;;;

(define (increment-where-to-encode wav)
    (set-wavfile-encode-channel! wav (+ 1 (wavfile-encode-channel wav)))
    (when (= (wavfile-encode-channel wav) (wavfile-channels wav))
          (set-wavfile-encode-channel! wav 0)
          (set-wavfile-encode-sample! wav (+ (wavfile-encode-sample wav) samples-per-fft))))

;;;;;;;;;;;;;;;;;;
;;; Given a wavfile, return a copy of the next set of samples to encode

(define (get-next-encode-samples wav)
    (increment-where-to-encode wav)
    (vector-copy (vector-ref (wavfile-samples wav) (wavfile-encode-channel wav))
                 (wavfile-encode-sample wav)
                 (+ (wavfile-encode-sample wav) samples-per-fft)))

;;;;;;;;;;;;;;;;;;
;;; Given a wavfile and a vector of samples, overwrite the next set of samples to encode with the given samples

(define (set-next-encode-samples wav samples)
    (vector-copy! (vector-ref (wavfile-samples wav) (wavfile-encode-channel wav))
                  (wavfile-encode-sample wav)
                  samples))

;;;;;;;;;;;;;;;;;;
;;; Given a bytestring, create a wavfile struct
;;; bytes - the bytestring to convert to a wavfile

(define (bytes->wavfile bytes is-wav)
   (let [(wav (call-with-values (lambda () (parse-wave-header bytes))
                                (lambda (e af c sr by ba bps s cs)
                                        (create-wavfile is-wav e af c sr by ba bps s cs))))]
        (set-wavfile-samples bytes wav)
        wav))

;;;;;;;;;;;;;;;;;;
;;; Writes the header of the given wavfile to the given bytestring

(define (write-wavfile-header-to-bytes wav bytes)
        (bytes-set! bytes 0 (char->integer #\R))
        (bytes-set! bytes 1 (char->integer #\I))
        (bytes-set! bytes 2 (char->integer #\F))
        (bytes-set! bytes 3 (char->integer #\F))
        (bytes-copy! bytes 4 (value->bytes (+ 36 (wavfile-chunksize wav)) 'little 4))
        (bytes-set! bytes 8 (char->integer #\W))
        (bytes-set! bytes 9 (char->integer #\A))
        (bytes-set! bytes 10 (char->integer #\V))
        (bytes-set! bytes 11 (char->integer #\E))
        (bytes-set! bytes 12 (char->integer #\f))
        (bytes-set! bytes 13 (char->integer #\m))
        (bytes-set! bytes 14 (char->integer #\t))
        (bytes-set! bytes 15 (char->integer #\space))
        (bytes-copy! bytes 16 (value->bytes 16 'little 4))
        (bytes-copy! bytes 20 (value->bytes (wavfile-audioformat wav) 'little 2))
        (bytes-copy! bytes 22 (value->bytes (wavfile-channels wav) 'little 2))
        (bytes-copy! bytes 24 (value->bytes (wavfile-samplerate wav) 'little 4))
        (bytes-copy! bytes 28 (value->bytes (wavfile-byterate wav) 'little 4))
        (bytes-copy! bytes 32 (value->bytes (wavfile-blockalign wav) 'little 2))
        (bytes-copy! bytes 34 (value->bytes (* (wavfile-bytespersample wav) 8) 'little 2))
        (bytes-set! bytes 36 (char->integer #\d))
        (bytes-set! bytes 37 (char->integer #\a))
        (bytes-set! bytes 38 (char->integer #\t))
        (bytes-set! bytes 39 (char->integer #\a))
        (bytes-copy! bytes 40 (value->bytes (wavfile-chunksize wav) 'little 4)))

;;;;;;;;;;;;;;;;;;
;;; Given a bytestring and a wavfile struct, rewrite *destructively* the wav samples to the bytestring
;;; wav - the wavfile of whos samples to write
;;; bytes - the bytes of which to write the wavfile samples to

(define (write-wavfile-to-bytes wav bytes)
    (for [(channel (wavfile-channels wav))]
         (write-wavfile-bytes-for-channel bytes wav channel)))

;;;;;;;;;;;;;;;;;;
;;; Given a channel and a wavfile, return the very first byte to start writing or reading to/from that channel in the given wav
(define (get-starting-byte channel wav)
    (+ (wavfile-chunkstart wav) (* (wavfile-bytespersample wav) channel)))

;;;;;;;;;;;;;;;;;;
;;; Given a byte index and a wav, get the next byte to write to
(define (get-next-byte byte wav)
    (+ byte (wavfile-blockalign wav)))

;;;;;;;;;;;;;;;;;;
;;; Writes *destructively* the given vector of samples to the bytes
;;; bytes - a bytestring to write to
;;; wav - the wavfile to retrieve the samples from
;;; channel - the channel to write to

(define (write-wavfile-bytes-for-channel bytes wav channel)
    (let [(samples (vector-ref (wavfile-samples wav) channel))]
         (do [(sample 0 (+ 1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length samples))]
             (bytes-copy! bytes byte (integer->integer-bytes (vector-ref samples sample)
                                                             (wavfile-bytespersample wav)
                                                             #t
                                                             (is-big-endian? wav))))))

;;;;;;;;;;;;;;;;;;
;;; Sets the vector representing the samples of either the left or right channel
;;; bytes - a bytestring representing wav file data
;;; wav - the wavfile to write to
;;; channel - the channel to convert

(define (set-wavfile-samples-for-channel bytes wav channel)
    (let [(samples (vector-ref (wavfile-samples wav) channel))]
         (do [(sample 0 (+ 1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length samples))]
             (vector-set! samples sample (integer-bytes->integer bytes
                                                                 #t
                                                                 (is-big-endian? wav)
                                                                 byte
                                                                 (+ (wavfile-bytespersample wav)
                                                                    byte))))))

;;;;;;;;;;;;;;;;;;
;;; Given a bytestring and a wavfile struct, fill in the samples for each channel
;;; bytes - the bytestring to pull the samples out of
;;; wav - the wavfile to set the samples of

(define (set-wavfile-samples bytes wav)
    (for [(channel (wavfile-channels wav))]
         (set-wavfile-samples-for-channel bytes wav channel)))

