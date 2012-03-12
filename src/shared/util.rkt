;Given a byte string bs, determine if bs represents a .wav file
(define (is-wav? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\R)
            (check 1 #\I)
            (check 2 #\F)
            (or (check 3 #\F) (check 3 #\X))
            (check 8 #\W)
            (check 9 #\A)
            (check 10 #\V)
            (check 11 #\E))))

;Given a byte string bs, determine if bs represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
(define (is-mp3? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\I)
            (check 1 #\D)
            (check 2 #\3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         ;;;;;;;;;
;;;;;;;;     Reading and writing of WAV files    ;;;;;;;;;
;;;;;;;;                                         ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A holder for all the data needed to recreate the wav file
(struct wavfile (endianess audioformat channels samplerate byterate blockalign bitspersample chunkstart chunksize samples))


;;; Writes *destructively* the given vector of samples to the bytes
;;; bytes - a bytestring to write to
;;; wav - the wavfile to retrieve the samples from
;;; channel - the channel to write to
(define (write-wavfile-bytes-for-channel bytes wav channel)
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
(define (set-wavfile-samples bytes wav)
    (letrec ((set-next-channel (lambda (channel)
                                       (when (not (= channel (wavfile-channels wav)))
                                             (begin (set-wavfile-samples-for-channel bytes wav channel)
                                                    (set-next-channel (+ 1 channel)))))))
            (set-next-channel 0)))

;;; Given a bytestring and a wavfile struct, rewrite *destructively* the wav samples to the bytestring
(define (write-wavfile-bytes bytes wav)
    (letrec ((set-next-channel (lambda (channel)
                                       (when (not (= channel (wavfile-channels wav)))
                                             (begin (write-wavfile-bytes-for-channel bytes wav channel)
                                                    (set-next-channel (+ 1 channel)))))))
            (set-next-channel 0)))


;; Inits the samples of the given wavfile so they can be set from the file's bytes
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


;;; Given a file, create a wavfile struct
;;; Requires - shared/fileio.rkt, shared/wav.sps
;;; TODO - when not wav, convert to wav
(define (make-wavfile file)
    (let ((bytes (read-file-into-bytestring file)))
         (if (is-wav? bytes)
             (let ((wav (call-with-values (lambda () (parse-wave-header bytes))
                                          (lambda (e af c sr by ba bps s cs) 
                                                  (wavfile e af c sr by ba bps s (- cs s) (make-vector c))))))
                  (init-samples wav)
                  (set-wavfile-samples bytes wav)
                  wav)
             (error "File is not a wav file"))))
