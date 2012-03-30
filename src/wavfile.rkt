(load "src/fileio.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; A holder for all the data needed to recreate the wav file
(struct wavfile (input output dest byteperpage
                 endianess audioformat channels samplerate byterate
                 blockalign bytespersample chunkstart chunksize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Wavfile Creation and Finalization
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Return a wavfile representation of the given in file set to write to the given out file
(define (file->wavfile src dest)
    (if (is-wav? src)
        (let [(out (if (null? dest) null (open-file-output-port dest)))]
             (create-wavfile-from-ports (open-file-input-port src) out null))
        (let [(out (if (null? dest) null (open-file-output-port tmpdest)))]
             (mp3->wav src tmpsrc)
             (create-wavfile-from-ports (open-file-input-port tmpsrc) out dest))))

;;;;;;;;;;;;;;;;;;
(define (finalize-wavfile wav)
   (when (not (null? (wavfile-output wav)))
         (pipe-remaining-bytes (wavfile-input wav) (wavfile-output wav))
         (when (not (null? (wavfile-dest wav)))
               (wav->mp3 tmpdest (wavfile-dest wav))))
   (void))

;;;;;;;;;;;;;;;;;;
(define (create-wavfile-from-ports in out dest)
    (call-with-values (lambda () (parse-wave-header (read-wavfile-header in)))
                      (lambda (e af c sr br ba bps s cs) (create-wavfile in out dest e af c sr br ba bps s cs))))

;;;;;;;;;;;;;;;;;;
;;; Common method to create a wavfile
(define (create-wavfile in out dest e af c sr br ba bps s cs)
    (let* [(bytes-per-sample (/ bps 8))
           (chunk-size (- cs s))
           (bytes-per-page (* bytes-per-sample c samples-per-fft))]
          (wavfile in out dest bytes-per-page e af c sr br ba bytes-per-sample s chunk-size)))

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
(define (read-samples wav)
    (bytes->samples (read-bytes-from-file (wavfile-byteperpage wav) (wavfile-input wav)) wav))

;;;;;;;;;;;;;;;;;;
(define (write-samples samples wav)
    (when (not (null? (wavfile-output wav)))
          (write-bytes-to-file (samples->bytes samples wav) (wavfile-output wav))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Wavfile Bytes -> Samples
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (bytes->samples bytes wav)
    (let [(samples (make-vector (wavfile-channels wav)))]
         (for [(channel (wavfile-channels wav))]
              (vector-set! samples channel (get-samples-for-channel bytes wav channel)))
         samples))

;;;;;;;;;;;;;;;;;;
(define (get-samples-for-channel bytes wav channel)
    (let [(samples (make-vector samples-per-fft))]
         (do [(sample 0 (add1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length samples))]
             (vector-set! samples sample (integer-bytes->integer bytes
                                                                 #t
                                                                 (is-big-endian? wav)
                                                                 byte
                                                                 (+ byte (wavfile-bytespersample wav)))))
         samples))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Wavfile Samples -> Bytes
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (samples->bytes samples wav)
    (let [(bytes (make-bytes (wavfile-byteperpage wav)))]
         (for [(channel (wavfile-channels wav))]
              (write-bytes-for-channel samples bytes wav channel))
         bytes))

;;;;;;;;;;;;;;;;;;
(define (write-bytes-for-channel samples bytes wav channel)
    (let [(s (vector-ref samples channel))]
         (do [(sample 0 (add1 sample))
              (byte (get-starting-byte channel wav) (get-next-byte byte wav))]
             [(= sample (vector-length s))]
             (bytes-copy! bytes byte (integer->integer-bytes (vector-ref s sample)
                                                             (wavfile-bytespersample wav)
                                                             #t
                                                             (is-big-endian? wav))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (get-wavfile-max-payload-size wav)
    (div (- (wavfile-chunksize wav) 
            (* 8 4 (wavfile-bytespersample wav) samples-per-fft))
         (* 8 (wavfile-bytespersample wav) samples-per-fft)))

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

