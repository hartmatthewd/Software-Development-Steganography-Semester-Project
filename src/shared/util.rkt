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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extracted from: /course/cs4500wc/Examples/FFT/fft.sls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2010 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rnrs/arithmetic/bitwise-6)
(require rnrs/arithmetic/fixnums-6)
(require rnrs/base-6)

(define i2pi
  (* 2.0 (acos -1.0) +1.0i))

(define (fft a)
  (fft-in-place (bit-reverse-copy a) i2pi))

(define (fft-inverse a)
  (let ((a (fft-in-place (bit-reverse-copy a) (- i2pi))))
    (do ((n (vector-length a))
         (i 0 (+ i 1)))
        ((= i n)
         a)
      (vector-set! a i (/ (vector-ref a i) n)))))

(define (bit-reverse-copy a)
  (let* ((n (vector-length a))
         (lgn (bitwise-length (- n 1)))
         (a2 (make-vector n 0.0)))
    (do ((k 0 (+ k 1)))
        ((= k n)
         a2)
      (vector-set! a2 (fxreverse-bit-field k 0 lgn) (vector-ref a k)))))

(define (fft-in-place a i2pi)
  (let* ((n (vector-length a))
         (lgn (bitwise-length (- n 1))))
    (do ((m 2 (+ m m)))
        ((> m n)
         a)
      (let ((omega_m (exp (/ i2pi m))))
        (do ((k 0 (+ k m)))
            ((= k n))
          (let ((m/2 (div m 2)))
            (do ((omega 1.0 (* omega omega_m))
                 (j 0 (+ j 1)))
                ((= j m/2))
              (let* ((t (* omega (vector-ref a (+ k j m/2))))
                     (u (vector-ref a (+ k j))))
                (vector-set! a (+ k j) (+ u t))
                (vector-set! a (+ k j m/2) (- u t))))))))))

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
