;;;
;;; A set of various utility methods
;;;
;;; REQUIRES:
;;;   shared/fileio.rkt
;;;   shared/wav.sps
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     Reading and writing of WAV files    
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A holder for all the data needed to recreate the wav file
(struct wavfile (endianess audioformat channels samplerate byterate blockalign bitspersample chunkstart chunksize samples))


;;; Given a bytestring, create a wavfile struct
;;; bytes - the bytestring to convert to a wavfile
;;; TODO - when bytes not wav, convert to wav before creating wavfile

(define (bytes->wavfile bytes)
   (if (is-wav? bytes)
       (let ((wav (call-with-values (lambda () (parse-wave-header bytes))
                                    (lambda (e af c sr by ba bps s cs) 
                                            (wavfile e af c sr by ba bps s (- cs s) (make-vector c))))))
            (init-samples wav)
            (set-wavfile-samples bytes wav)
            wav)
       (error "File is not a wav file")))


;;; Given a bytestring and a wavfile struct, rewrite *destructively* the wav samples to the bytestring
;;; wav - the wavfile of whos samples to write
;;; bytes - the bytes of which to write the wavfile samples to

(define (write-wavfile-to-bytes wav bytes)
    (letrec ((set-next-channel (lambda (channel)
                                       (when (not (= channel (wavfile-channels wav)))
                                             (begin (write-wavfile-bytes-for-channel bytes wav channel)
                                                    (set-next-channel (+ 1 channel)))))))
            (set-next-channel 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     Locals
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Writes *destructively* the given vector of samples to the bytes
;;; bytes - a bytestring to write to
;;; wav - the wavfile to retrieve the samples from
;;; channel - the channel to write to

(define (write-wavfile-bytes-for-channel bytes wav channel)

    ;;; samples - a vector of samples
    ;;; get-first-byte - a function that, given a sample, returns the first byte (assumes 2 byte samples)
    ;;; get second byte - a function that, given a sample, returns the second byte (assumes 2 byte samples)
    ;;; set-next-byte - sets the next 2 bytes of the given bytestring to the byte representation of the next sample

    (letrec ((samples (vector-ref (wavfile-samples wav) 
                                  channel))
             (get-first-byte (if (equal? (wavfile-endianess wav) 'little)
                                 (lambda (sample) (modulo sample 256))
                                 (lambda (sample) (floor (/ sample 256)))))
             (get-second-byte (if (equal? (wavfile-endianess wav) 'little)
                                  (lambda (sample) (floor (/ sample 256)))
                                  (lambda (sample) (modulo sample 256))))
             (set-next-byte (lambda (byte sample)
                                    (when (not (= sample (vector-length samples)))
                                          (begin (bytes-set! bytes byte       (get-first-byte (vector-ref samples sample)))
                                                 (bytes-set! bytes (+ byte 1) (get-second-byte (vector-ref samples sample)))
                                                 (set-next-byte (+ byte (wavfile-blockalign wav)) (+ sample 1)))))))
            (set-next-byte (+ (wavfile-chunkstart wav) (* (/ (wavfile-bitspersample wav) 8) channel)) 0)))



;;; Sets the vector representing the samples of either the left or right channel
;;; bytes - a bytestring representing wav file data
;;; wav - the wavfile to write to
;;; channel - the channel to convert

(define (set-wavfile-samples-for-channel bytes wav channel)

    ;;; get-sample-value - given 2 bytes that make up a sample (assumes 2 byte samples), return the sample value
    ;;; set-next-sample - sets the next sample to the value created by the next 2 bytes

    (letrec ((samples (vector-ref (wavfile-samples wav) channel))
             (get-sample-value (if (equal? (wavfile-endianess wav) 'little)
                                   (lambda (a b) (+ a (* 256 b)))
                                   (lambda (a b) (+ (* 256 a) b))))
             (set-next-sample (lambda (byte sample)
                                      (when (not (= sample (vector-length samples)))
                                            (begin (vector-set! samples 
                                                                sample 
                                                                (get-sample-value (bytes-ref bytes byte) 
                                                                                  (bytes-ref bytes (+ byte 1))))
                                                   (set-next-sample (+ byte (wavfile-blockalign wav)) (+ sample 1)))))))
            (set-next-sample (+ (wavfile-chunkstart wav) (* (/ (wavfile-bitspersample wav) 8) channel)) 0)))



;;; Given a bytestring and a wavfile struct, fill in the samples for each channel
;;; bytes - the bytestring to pull the samples out of
;;; wav - the wavfile to set the samples of

(define (set-wavfile-samples bytes wav)
    (letrec ((set-next-channel (lambda (channel)
                                       (when (not (= channel (wavfile-channels wav)))
                                             (begin (set-wavfile-samples-for-channel bytes wav channel)
                                                    (set-next-channel (+ 1 channel)))))))
            (set-next-channel 0)))


;;; Inits the samples of the given wavfile so they can be set from the file's bytes
;;; wav - inits the sample vectors to empty vectors

(define (init-samples wav)
    (letrec ((doit (lambda (channel)
                           (when (< channel (wavfile-channels wav))
                                 (begin (vector-set! (wavfile-samples wav) channel (make-vector (/ (wavfile-chunksize wav) 
                                                                                                   (* (/ (wavfile-bitspersample wav) 
                                                                                                         8) 
                                                                                                      (wavfile-channels wav)))
                                                                                                0))
                                        (doit (+ channel 1)))))))
            (doit 0)))


