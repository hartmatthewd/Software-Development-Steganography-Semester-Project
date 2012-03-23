;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Reading and writing of WAV files
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; A holder for all the data needed to recreate the wav file
(struct wavfile (is-wav endianess audioformat channels samplerate byterate blockalign bytespersample chunkstart chunksize samples))


;;;;;;;;;;;;;;;;;;
;;; Return a wavfile representation of the given file

(define (file->wavfile file)
    (let* [(is-wav (is-wav? file))
           (do-conversion (lambda (f) (bytes->wavfile (read-file-into-bytestring f) is-wav)))]
          (if is-wav
              (do-conversion file)
              (begin (mp3->wav file tmp-file)
                     (do-conversion tmp-file)))))

;;;;;;;;;;;;;;;;;;
;;; Writes the given wavfile to the given file

(define (wavfile->file wav file)
    (let ((bytes (make-bytes (+ (wavfile-chunkstart wav) (wavfile-chunksize wav)))))
         (write-wavfile-header-to-bytes wav bytes)
         (write-wavfile-to-bytes wav bytes)
         (write-bytestring-to-file bytes tmp-file))
    (if (wavfile-is-wav wav)
        (copy-file tmp-file file #t)
        (wav->mp3 tmp-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     File I/O
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Attepts to read the given file, returning a bytestring containing the file's bytes
; If the file does not exist, throws an error
(define (read-file-into-bytestring file)
   (if (file-exists? file)
       (file->bytes file)
       (error (string-append "File " file " does not exist"))))

;;;;;;;;;;;;;;;;;;
; Writes the given bytevector to a file of the given name
(define (write-bytestring-to-file bytestring file)
   (when (file-exists? file)
       (delete-file file))
   (display-to-file bytestring file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     MP3 and WAV
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;Given a file, determine if the file represents a .wav file
(define (is-wav? file)
    (let* ((bs (read-bytes 12 (open-input-file file)))
           (check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\R)
            (check 1 #\I)
            (check 2 #\F)
            (or (check 3 #\F) (check 3 #\X))
            (check 8 #\W)
            (check 9 #\A)
            (check 10 #\V)
            (check 11 #\E))))

;;;;;;;;;;;;;;;;;;
;Given a file, determine if the file represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
(define (is-mp3? file)
    (let* ((bs (read-bytes 3 (open-input-file file)))
           (firsttwo (bitwise-and (+ (* 256 (bytes-ref bs 0)) (bytes-ref bs 1)) 65534)) ;65534 = 0xFFFE
           (check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
          (or (and (check 0 #\I)
                   (check 1 #\D)
                   (check 2 #\3))
              (or (= firsttwo 65530)     ;0xFFFA MP3 v1.0
                  (= firsttwo 65522)     ;0xFFF2 MP3 v2.0
                  (= firsttwo 65506))))) ;0xFFE2 MP3 v2.5


;;;;;;;;;;;;;;;;;;
;;; Given an input wav file and an output mp3 path, convert the wav to mp3 via the LAME encoder and output it at the given mp3 output path

(define (wav->mp3 wav mp3)
   (system (string-append lame-path " " wav " " mp3)))

;;;;;;;;;;;;;;;;;;
;;; Given an input mp3 file and an output wav path, convert the mp3 to wav via the LAME encoder and output it at the given wav output path

(define (mp3->wav mp3 wav)
   (system (string-append lame-path " --decode " mp3 " " wav)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Locals
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a bytestring, create a wavfile struct
;;; bytes - the bytestring to convert to a wavfile

(define (bytes->wavfile bytes is-wav)
   (let ((wav (call-with-values (lambda () (parse-wave-header bytes))
                                (lambda (e af c sr by ba bps s cs)
                                        (wavfile is-wav e af c sr by ba (/ bps 8) s (- cs s) (make-vector c))))))
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
    (let [(samples-per-channel (/ (wavfile-chunksize wav)
                                  (* (wavfile-bytespersample wav)
                                     (wavfile-channels wav))))]
         (for [(channel (wavfile-channels wav))]
              (vector-set! (wavfile-samples wav) channel (make-vector samples-per-channel))
              (set-wavfile-samples-for-channel bytes wav channel))))
