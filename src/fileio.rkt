;;;
;;; Functions handling reading the writing of files. Also handles transformation from wav to mp3 and back
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;     Reading and writing of WAV files
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A holder for all the data needed to recreate the wav file
(struct wavfile (endianess audioformat channels samplerate byterate blockalign bitspersample chunkstart chunksize samples))


;;; Return a wavfile representation of the given file

(define (file->wavfile file)
    (bytes->wavfile (read-file-into-bytestring (ensure-is-wav file))))


;;; Writes the given wavfile to the given file

(define (wavfile->file wav file)
    (let ((bytes (make-bytes (+ chunkstart chunksize))))
         (write-wavfile-header-to-bytes wav bytes)
         (write-wavfile-to-bytes wav bytes)
         (write-bytestring-to-file bytes file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     File I/O
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Attepts to read the given file, returning a bytestring containing the file's bytes
; If the file does not exist, throws an error
(define (read-file-into-bytestring file)
   (if (file-exists? file)
       (file->bytes file)
       (error (string-append "File " file " does not exist"))))

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

;;; If the given file is a wav file, return it, else convert it to wav and return the converted file

(define (ensure-is-wav file)
    (if (is-wav? file)
        file
        (let ((tmpfile "_temp.wav"))
             (begin (mp3->wav file tmpfile)
                    tmpfile))))

;;; If the given file is a mp3 file, return it, else convert it to mp3 and return the converted file

(define (ensure-is-mp3 file)
    (if (is-mp3? file)
        file
        (let ((tmpfile "_temp.mp3"))
             (begin (wav->mp3 file tmpfile)
                    tmpfile))))

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

;Given a file, determine if the file represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
(define (is-mp3? file)
    (let* ((bs (read-bytes 3 (open-input-file file)))
           (check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\I)
            (check 1 #\D)
            (check 2 #\3))))

(define lame-path "/course/cs4500wc/bin/lame")

;;; Given an input wav file and an output mp3 path, convert the wav to mp3 via the LAME encoder and output it at the given mp3 output path

(define (wav->mp3 wav mp3)
   (system (string-append lame-path " " wav " " mp3)))

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

(require rnrs/bytevectors-6)
(require rnrs/base-6)

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

;;; Given a bytestring, create a wavfile struct
;;; bytes - the bytestring to convert to a wavfile

(define (bytes->wavfile bytes)
   (if (is-wav? bytes)
       (let ((wav (call-with-values (lambda () (parse-wave-header bytes))
                                    (lambda (e af c sr by ba bps s cs)
                                            (wavfile e af c sr by ba bps s (- cs s) (make-vector c))))))
            (init-samples wav)
            (set-wavfile-samples bytes wav)
            wav)
       (error "File is not a wav file")))


;;; Writes the header of the given wavfile to the given bytestring

(define (write-wavfile-header-to-bytes wav bytes)
        (bytes-set! bytes 0 (char->integer #\R))
        (bytes-set! bytes 1 (char->integer #\I))
        (bytes-set! bytes 2 (char->integer #\F))
        (bytes-set! bytes 3 (char->integer #\F))
        
)


;;; Given a bytestring and a wavfile struct, rewrite *destructively* the wav samples to the bytestring
;;; wav - the wavfile of whos samples to write
;;; bytes - the bytes of which to write the wavfile samples to

(define (write-wavfile-to-bytes wav bytes)
    (letrec ((set-next-channel (lambda (channel)
                                       (when (not (= channel (wavfile-channels wav)))
                                             (begin (write-wavfile-bytes-for-channel bytes wav channel)
                                                    (set-next-channel (+ 1 channel)))))))
            (set-next-channel 0)))

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

