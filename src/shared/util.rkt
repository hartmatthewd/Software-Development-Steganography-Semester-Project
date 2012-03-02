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


;;; Returns a vector representing the samples of either the left or right channel
;;; bytes - a bytestring representing wav file data
;;; start - the starting byte of the data segment
;;; channels - the number of channels in the wav file
;;; channel - the channel to convert
;;; endianess - the endianness of the wav data
;;; TODO - this assumes 2 byte samples, maybe change this

(define (get-sample-vector-from-bytes bytes start channels channel endianess)
    (letrec ((samples (/ (- (bytes-length bytes) start) (* 2 channels)))
             (vector (make-vector samples))
             (get-sample-value (if (equal? endianess 'little)
                                   (lambda (a b) (+ a (* 256 b)))
                                   (lambda (a b) (+ (* 256 a) b))))
             (set-next-sample (lambda (byte sample)
                                      (if (= sample samples)
                                          vector
                                          (begin (vector-set! vector sample (get-sample-value (bytes-ref bytes byte) (bytes-ref bytes (+ byte 1))))
                                                 (set-next-sample (+ byte (* 2 channels)) (+ sample 1)))))))
            (set-next-sample (+ start channel) 0)))

;;; A holder for all the data needed to recreate the wav file
(struct wavfile (endianess audioformat channels samplerate byterate blockalign bitspersample chunkstart chunksize lsamples rsamples) #:mutable)

;;; Given a file, create a wavfile structi
;;; Requires - shared/fileio.rkt, shared/wav.sps
;;; TODO - when not wav, convert to wav
(define (make-wavfile file)
    (let ((bytes (read-file-into-bytestring file)))
         (if (is-wav? bytes)
             (let ((wav (call-with-values (lambda () (parse-wave-header bytes))
                                          (lambda (e af c sr by ba bps s cs) 
                                                  (wavfile e af c sr by ba bps s cs null null)))))
                  (set-wavfile-lsamples! wav (get-sample-vector-from-bytes bytes (wavfile-chunkstart wav) 
                                                                                 (wavfile-channels wav) 
                                                                                 0 
                                                                                 (wavfile-endianess wav)))
                  (if (= 2 (wavfile-channels wav))
                      (set-wavfile-lsamples! wav (get-sample-vector-from-bytes bytes (wavfile-chunkstart wav) 
                                                                                     (wavfile-channels wav) 
                                                                                     1 
                                                                                     (wavfile-endianess wav)))
                      '())
                  wav)
             (error "File is not a wav file"))))
