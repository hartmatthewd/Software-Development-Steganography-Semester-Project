;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; A holder for all the data needed to recreate the wav file
(struct wavfile (input output dest [encode-channel #:mutable]
                 endianess audioformat channels samplerate byterate
                 blockalign bytespersample chunkstart chunksize 
                 bytes samples))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Utils
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Returns whether or not the wav is large enough to hold the payload

(define (ensure-wav-large-enough? wav size)
    (let [(maxsize (div (div (div (wavfile-chunksize wav) (wavfile-bytespersample wav)) samples-per-fft) 8))]
         (when (not (<= size maxsize))
               (error 'Failure (format "Payload is too large for the given audio carrier. Size: ~a bytes | Max Size: ~a bytes" size maxsize)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile Creation and Finalization
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Return a wavfile representation of the given in file set to write to the given out file
(define (file->wavfile-encoder src dest)
    (if (is-wav? src)
        (create-and-initialize-wavfile (open-file-input-port src) (open-file-output-port dest) null)
        (begin (mp3->wav src tmpsrc)
               (create-and-initialize-wavfile (open-file-input-port tmpsrc) (open-file-output-port tmpdest) dest))))

;;;;;;;;;;;;;;;;;;
(define (file->wavfile-decoder src)
    (if (is-wav? src)
        (create-and-initialize-wavfile (open-file-input-port src) null null)
        (begin (mp3->wav src tmpsrc)
               (create-and-initialize-wavfile (open-file-input-port tmpsrc) null null))))

;;;;;;;;;;;;;;;;;;
(define (finalize-wavfile wav)
   (when (not (null? (wavfile-output wav)))
         (write-current-samples wav)
         (pipe-remaining-bytes (wavfile-input wav) (wavfile-output wav))
         (when (not (null? (wavfile-dest wav)))
               (wav->mp3 tmpdest (wavfile-dest wav))))
   (void))

;;;;;;;;;;;;;;;;;;
(define (create-and-initialize-wavfile in out dest)
    (let [(wav (call-with-values (lambda () (parse-wave-header (read-wavfile-header in)))
                                 (lambda (e af c sr br ba bps s cs) (create-wavfile in out dest e af c sr br ba bps s cs))))]
         (when (not (null? out))
               (write-wavfile-header wav))
         (read-next-wavfile-samples wav)
         wav))

;;;;;;;;;;;;;;;;;;
;;; Common method to create a wavfile
(define (create-wavfile in out dest e af c sr br ba bps s cs)
    (let* [(starting-channel (- 0 1))
           (bytes-per-sample (/ bps 8))
           (chunk-size (- cs s))
           (bytes-per-page (* bytes-per-sample c samples-per-fft))
           (wav (wavfile in out dest starting-channel e af c sr br ba bytes-per-sample s chunk-size (make-bytes bytes-per-page) (make-vector c)))]
         (vector-map! (lambda (s) (make-vector samples-per-fft)) (wavfile-samples wav))
         wav))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile I/O
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (read-wavfile-header in)
    (read-bytes-from-file 44 in))

;;;;;;;;;;;;;;;;;;
(define (write-wavfile-header wav)
    (write-bytes-to-file (create-wavfile-header-bytes wav) (wavfile-output wav)))

;;;;;;;;;;;;;;;;;;
(define (page-wavfile-samples wav)
    (when (not (null? (wavfile-output wav)))
          (write-current-samples wav))
    (read-next-wavfile-samples wav))

;;;;;;;;;;;;;;;;;;
(define (read-next-wavfile-samples wav)
    (read-next-bytes wav)
    (parse-wavfile-bytes-into-samples wav))

;;;;;;;;;;;;;;;;;;
(define (write-current-samples wav)
    (rewrite-wavfile-bytes wav)
    (write-current-bytes wav))

;;;;;;;;;;;;;;;;;;
(define (read-next-bytes wav)
    (bytes-copy! (wavfile-bytes wav) 0 (read-bytes-from-file (bytes-length (wavfile-bytes wav)) (wavfile-input wav))))

;;;;;;;;;;;;;;;;;;
(define (write-current-bytes wav)
    (write-bytes-to-file (wavfile-bytes wav) (wavfile-output wav)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Wavfile Bytes -> Samples
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Wavfile, fill in the samples for each channel of the wavfile from the bytes of the wavfile
;;; wav - the wavfile to set the samples of

(define (parse-wavfile-bytes-into-samples wav)
    (for [(channel (wavfile-channels wav))]
         (parse-wavfile-samples-for-channel wav channel)))

;;;;;;;;;;;;;;;;;;
;;; Sets the vector representing the samples of either the left or right channel
;;; bytes - a bytestring representing wav file data
;;; wav - the wavfile to write to
;;; channel - the channel to convert

(define (parse-wavfile-samples-for-channel wav channel)
    (let [(samples (vector-ref (wavfile-samples wav) channel))]
         (do [(sample 0 (add1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length samples))]
             (vector-set! samples sample (integer-bytes->integer (wavfile-bytes wav)
                                                                 #t
                                                                 (is-big-endian? wav)
                                                                 byte
                                                                 (+ byte (wavfile-bytespersample wav)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Wavfile Samples -> Bytes
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a bytestring and a wavfile struct, rewrite *destructively* the wav samples to the bytestring
;;; wav - the wavfile of whos samples to write
;;; bytes - the bytes of which to write the wavfile samples to

(define (rewrite-wavfile-bytes wav)
    (for [(channel (wavfile-channels wav))]
         (rewrite-wavfile-bytes-for-channel wav channel)))

;;;;;;;;;;;;;;;;;;
;;; Writes *destructively* the given vector of samples to the bytes
;;; bytes - a bytestring to write to
;;; wav - the wavfile to retrieve the samples from
;;; channel - the channel to write to

(define (rewrite-wavfile-bytes-for-channel wav channel)
    (let [(samples (vector-ref (wavfile-samples wav) channel))]
         (do [(sample 0 (add1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length samples))]
             (bytes-copy! (wavfile-bytes wav) byte (integer->integer-bytes (vector-ref samples sample)
                                                                           (wavfile-bytespersample wav)
                                                                           #t
                                                                           (is-big-endian? wav))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Wavfile Encoding/Decoding
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a wavfile, return a copy of the next set of samples to encode

(define (get-next-samples wav)
    (set-wavfile-encode-channel! wav (add1 (wavfile-encode-channel wav)))
    (when (= (wavfile-encode-channel wav) (wavfile-channels wav))
          (set-wavfile-encode-channel! wav 0)
          (page-wavfile-samples wav))
    (vector-ref (wavfile-samples wav) (wavfile-encode-channel wav)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a channel and a wavfile, return the very first byte to start writing or reading to/from that channel in the given wav
(define (get-starting-byte channel wav)
    (* (wavfile-bytespersample wav) channel))

;;;;;;;;;;;;;;;;;;
;;; Given a byte index and a wav, get the next byte to write to
(define (get-next-byte byte wav)
    (+ byte (wavfile-blockalign wav)))

;;;;;;;;;;;;;;;;;;
;;; Creates a bytestring representing the header of the given wavfile

(define (create-wavfile-header-bytes wav)
    (let [(bytes (make-bytes 44))]
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
         (bytes-copy! bytes 40 (value->bytes (wavfile-chunksize wav) 'little 4))
         bytes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Extracted from: /course/cs4500wc/Examples/Wave/wav.sps
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

(define (parse-wave-header bvec)
  (if (not (>= (bytevector-length bvec) 44))
      (error 'parse-wave-header "WAVE file too short" bvec))
  (let* ((fetch (lambda (i) (bytevector-u8-ref bvec i)))
         (chunkID
          (apply string
                 (map integer->char
                      (map fetch '(0 1 2 3)))))
         (endianness (cond ((string=? chunkID "RIFF") 'little)
                           ((string=? chunkID "RIFX") 'big)
                           (else
                            (error 'parse-wave-header
                                   "bad ChunkID" chunkID))))
         (chunkSize (bytevector-u32-ref bvec 4 endianness))
         (format0
          (apply string
                 (map integer->char
                      (map fetch '(8 9 10 11)))))

         (subchunk1ID
          (apply string
                 (map integer->char
                      (map fetch '(12 13 14 15)))))
         (subchunk1Size (bytevector-u32-ref bvec 16 endianness))
         (audioFormat   (bytevector-u16-ref bvec 20 endianness))
         (numChannels   (bytevector-u16-ref bvec 22 endianness))
         (sampleRate    (bytevector-u32-ref bvec 24 endianness))
         (byteRate      (bytevector-u32-ref bvec 28 endianness))
         (blockAlign    (bytevector-u16-ref bvec 32 endianness))
         (bitsPerSample (bytevector-u16-ref bvec 34 endianness))

         (subchunk2ID
          (apply string
                 (map integer->char
                      (map fetch '(36 37 38 39)))))
         (subchunk2Size (bytevector-u32-ref bvec 40 endianness)))
    (cond ((and (string=? format0 "WAVE")
                (string=? subchunk1ID "fmt ")
                (= subchunk1Size 16)
                (= audioFormat 1)                ; PCM
                (<= 1 numChannels 2)             ; mono or stereo
                (= byteRate
                   (* sampleRate numChannels (div bitsPerSample 8)))
                (= blockAlign
                   (* numChannels (div bitsPerSample 8)))
                (= 0 (mod bitsPerSample 8))
                (string=? subchunk2ID "data")
                (= 0 (mod subchunk2Size blockAlign)))
           (values endianness
                   audioFormat
                   numChannels
                   sampleRate
                   byteRate
                   blockAlign
                   bitsPerSample
                   44
                   (+ 8 chunkSize)))
          (else
           (error 'parse-wave-header
                  "unrecognized or illegal WAVE format"
                  chunkID chunkSize format0
                  subchunk1ID subchunk1Size audioFormat numChannels
                  byteRate blockAlign bitsPerSample
                  subchunk2ID subchunk2Size)))))

