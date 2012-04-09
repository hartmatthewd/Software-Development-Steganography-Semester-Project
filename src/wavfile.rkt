(load "src/fileio.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Wavfile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; A holder for all the data needed to recreate the wav file
; input (input-port?) - an input port to read from
; bytesperpage (real?) - the number of bytes to write/read on every page
; endianes (symbol? -> 'little or 'big) - the endianess of the samples
; audioformat (real?) - the audio format
; channels (real?) - the number of channels
; samplerate (real?) - the number of samples per second per channel
; byterate (real?) - the number of bytes per second per channel
; blockalign (real?) - the difference, in bytes, between any two concurrent samples of the sample channel
; bytespersample (real?) - the number of bytes that represent each sample
; chunkstart (real?) - the start of the data segment in bytes
; chunksize (real?) - the size of the data segment in bytes

(struct wavfile (input bytesperpage endianess audioformat channels samplerate byterate blockalign bytespersample chunkstart chunksize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Wavfile Creation and Finalization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Return a wavfile representation of the given in file set to write to the given out file
; inputs
;     src (string?) - the source where to find a wav or mp3 file
; outputs
;     wavfile?

(define (file->wavfile src)
    (create-wavfile-from-port (open-file-input-port src)))

;;;;;;;;;;;;;;;;;;
; Creates a wavfile
; inputs
;     in (input-port?) - an input port to read from
; outputs
;     wavfile?

(define (create-wavfile-from-port in)
    (call-with-values (lambda () (parse-wave-header (read-wavfile-header in)))
                      (lambda (e af c sr br ba bps s cs) (create-wavfile in e af c sr br ba bps s cs))))

;;;;;;;;;;;;;;;;;;
; Common method to create a wavfile
; inputs
;     in (input-port?) - an input port to read from
;     e (symbol? -> 'little or 'big) - endianess
;     af (real?) - audio format
;     c (real?) - channels
;     sr (real?) - sample rate
;     br (real?) - byte rate
;     ba (real?) - block align
;     bps (real?) - bits per second
;     s (real?) - chunkstart
;     cs (real?) - chunksize
; outputs
;     wavfile?

(define (create-wavfile in e af c sr br ba bps s cs)
    (let* [(bytes-per-sample (/ bps 8))
           (chunk-size (- cs s))
           (bytes-per-page (* bytes-per-sample c samples-per-fft))]
          (wavfile in bytes-per-page e af c sr br ba bytes-per-sample s chunk-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Wavfile I/O
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Returns a bytestring for the header portion of the wavfile located in the given input port
; inputs
;     in (input-port?) - the given input port reading from a wave file
; outputs
;     bytes?

(define (read-wavfile-header in)
    (read-bytes-from-file 44 in))

;;;;;;;;;;;;;;;;;;
; Create and write wave file header bytes to the given output port
; inputs
;     wavfile (wavfile?) - the wavfile who's header to write
;     output (output-port?) - the output port to write to
; outputs
;     void

(define (write-wavfile-header wav output)
    (write-bytes-to-file (create-wavfile-header-bytes wav) output))

;;;;;;;;;;;;;;;;;;
; Reads the next set of samples from the source of the given wavfile
; inputs
;     wav (wavfile?) - the wavfile to read the next set of samples from
; outputs
;     vector?

(define (read-samples wav)
    (bytes->samples (read-bytes-from-file (wavfile-bytesperpage wav) (wavfile-input wav)) wav))

;;;;;;;;;;;;;;;;;;
; Writes the given samples to the given wavfile
; inputs
;     samples (vector?) - the vector of samples to write
;     wav (wavfile?) - the wavfile to write the given samples to
;     output (output-port?) - the output port to write to
; outputs
;     void

(define (write-samples samples wav output)
    (write-bytes-to-file (samples->bytes samples wav) output))

;;;;;;;;;;;;;;;;;;
; Given a wavfile and an output port, pipe the rest of the bytes from the wavfile to the output port
; inputs
;     wav (wavfile?) - the wavfile who's input port to read from
;     output (output-port?) - the output port to write to
; outputs
;     void

(define (pipe-remaining-wavfile-bytes wav output)
    (pipe-remaining-bytes (wavfile-input wav) output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Wavfile Bytes -> Samples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Converts the given bytes into a vector of samples for the given wavfile
; inputs
;     bytes (bytes?) - the bytestring to convert to samples
;     wav (wavfile?) - the wavfile whos samples the returned vector will represent
; outputs
;     vector?

(define (bytes->samples bytes wav)
    (let [(samples (make-vector (wavfile-channels wav)))]
         (for [(channel (wavfile-channels wav))]
              (vector-set! samples channel (get-samples-for-channel bytes wav channel)))
         samples))

;;;;;;;;;;;;;;;;;;
; Converts the given bytes into a vector of samples for the given wavfile for the given channel
; inputs
;     bytes (bytestring?) - the bytestring to convert to samples
;     wav (wavfile?) - the wavfile whos samples the returned vector will represent
;     channel (real?) - the channel of audio to convert (base 0)
; outputs
;     vecor?

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
;;
;;  Wavfile Samples -> Bytes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Converts the given samples into bytes for the given wavfile
; inputs
;     samples (vector?) - the vector of samples to convert into bytes
;     wav (wavfile?) - the wavfile whos bytes the returned bytestring will represent
; outputs
;     bytes?

(define (samples->bytes samples wav)
    (let [(bytes (make-bytes (wavfile-bytesperpage wav)))]
         (for [(channel (wavfile-channels wav))]
              (write-bytes-for-channel samples bytes wav channel))
         bytes))

;;;;;;;;;;;;;;;;;;
; Converts the given samples into bytes for the given wavfile for the given channel
; inputs
;     samples (vector?) - the vector of samples to convert into bytes
;     wav (wavfile?) - the wavfile whos bytes the returned bytestring will represent
;     channel (real?) - the channel of audio to convert (base 0)
; outputs
;     void

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
;;
;;  Utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Returns the maximum payload size of the given wavfile
; inputs
;     wav (wavfile?) - the wavfile to find the maximum payload size of
; outputs
;     real?

(define (get-wavfile-max-payload-size wav)
    (let [(bytes-per-byte (* 8 (wavfile-bytespersample wav) samples-per-fft))]
         (div (- (wavfile-chunksize wav) (* 4 bytes-per-byte))
              bytes-per-byte)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Locals
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a channel and a wavfile, return the very first byte to start writing or reading to/from that channel in the given wav
; inputs
;     channel (real?) - the channel that will be operated on
;     wav (wavfile?) - the wavfile whos starting data byte to find
; outputs
;     real?

(define (get-starting-byte channel wav)
    (* (wavfile-bytespersample wav) channel))

;;;;;;;;;;;;;;;;;;
; Given a byte index and a wav, get the next byte to write to
; inputs
;     byte (real?) - the current byte
;     wav (wavfile?) - the wavfile whos byte this will represent
; outputs
;     real?

(define (get-next-byte byte wav)
    (+ byte (wavfile-blockalign wav)))

;;;;;;;;;;;;;;;;;;
; Creates a bytestring representing the header of the given wavfile
; inputs
;     wav (wavfile?) - the wavfile whos header this will represent
; outputs
;     bytes?

(define (create-wavfile-header-bytes wav)
    (let [(bytes (make-bytes 44))]
         (bytes-set! bytes 0 (char->integer #\R))
         (bytes-set! bytes 1 (char->integer #\I))
         (bytes-set! bytes 2 (char->integer #\F))
         (bytes-set! bytes 3 (char->integer #\F))
         (bytes-copy! bytes 4 (integer->integer-bytes (+ 36 (wavfile-chunksize wav)) 4 #f #f))
         (bytes-set! bytes 8 (char->integer #\W))
         (bytes-set! bytes 9 (char->integer #\A))
         (bytes-set! bytes 10 (char->integer #\V))
         (bytes-set! bytes 11 (char->integer #\E))
         (bytes-set! bytes 12 (char->integer #\f))
         (bytes-set! bytes 13 (char->integer #\m))
         (bytes-set! bytes 14 (char->integer #\t))
         (bytes-set! bytes 15 (char->integer #\space))
         (bytes-copy! bytes 16 (integer->integer-bytes 16 4 #f #f))
         (bytes-copy! bytes 20 (integer->integer-bytes (wavfile-audioformat wav) 2 #f #f))
         (bytes-copy! bytes 22 (integer->integer-bytes (wavfile-channels wav) 2 #f #f))
         (bytes-copy! bytes 24 (integer->integer-bytes (wavfile-samplerate wav) 4 #f #f))
         (bytes-copy! bytes 28 (integer->integer-bytes (wavfile-byterate wav) 4 #f #f ))
         (bytes-copy! bytes 32 (integer->integer-bytes (wavfile-blockalign wav) 2 #f #f))
         (bytes-copy! bytes 34 (integer->integer-bytes (* (wavfile-bytespersample wav) 8) 2 #f #f))
         (bytes-set! bytes 36 (char->integer #\d))
         (bytes-set! bytes 37 (char->integer #\a))
         (bytes-set! bytes 38 (char->integer #\t))
         (bytes-set! bytes 39 (char->integer #\a))
         (bytes-copy! bytes 40 (integer->integer-bytes (wavfile-chunksize wav) 4 #f #f))
         bytes))

;;;;;;;;;;;;;;;;;;
; Returns true if the given wavfile bytes are big endian, false otherwise
; inputs
;     wav (wavfile?) - the wavfile of which to determine if is big endian or not
; outputs
;     boolean?

(define (is-big-endian? wav)
    (eq? (wavfile-endianess wav) 'big))

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

