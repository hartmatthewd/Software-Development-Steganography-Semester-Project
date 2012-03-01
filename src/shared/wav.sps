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
;
; Reading, displaying, and writing a WAVE File (WAV).
;
; This is an R6RS top-level program.  It has been tested in
; Larceny.
;
; Usage:
;
; larceny --r6rs --program wav.sch -- -summarize srcfile
; larceny --r6rs --program wav.sch -- --summarize srcfile
; larceny --r6rs --program wav.sch -- -copy srcfile dstfile
; larceny --r6rs --program wav.sch -- --copy srcfile dstfile
; larceny --r6rs --program wav.sch -- -extract time1 time2 srcfile dstfile
; larceny --r6rs --program wav.sch -- --extract time1 time2 srcfile dstfile
;
; where
;
; srcfile is an existing WAVE file
; dstfile is the name of the WAVE file to be created
; time1 is an offset in seconds from the beginning of the recording
; time2 is an offset in seconds from the beginning of the recording
;     and the section to be extracted goes from time1 to time2
;
; FIXME:
; For the --extract operation, the source WAVE file must already
; be in the minimal form produced by the --copy operation.

#!r6rs
(library (local wav)

(export summarize-wave-file
        scrub-wave-file
        scrub-wave-port
        extract-from-wave-file
        parse-wave-file-header
        parse-wave-header)

(import (rnrs base)
        (rnrs bytevectors)
        (rnrs control)
        (rnrs io ports)
        (rnrs io simple)
        (rnrs files)
        (rnrs programs)
        (rnrs arithmetic fixnums))

;(define cmds (cdr (command-line)))

;(define operation
;  (cond ((null? cmds)
;         'usage)
;        ((and (or (string=? (car cmds) "-summarize")
;                  (string=? (car cmds) "--summarize"))
;              (= (length cmds) 2))
;         'summarize)
;        ((and (or (string=? (car cmds) "-copy")
;                  (string=? (car cmds) "--copy"))
;              (= (length cmds) 3))
;         'copy)
;        ((and (or (string=? (car cmds) "-extract")
;                  (string=? (car cmds) "--extract"))
;              (= (length cmds) 5))
;         'extract)
;        (else
;         'usage)))

;(define (print-usage)
;  (define msg0
;    "Usage:")
;  (define msg1
;    "larceny --r6rs --program wav.sps -- -copy srcfile dstfile")
;  (define msg2
;    "larceny --r6rs --program wav.sps -- -summarize srcfile")
;  (define msg3
;    "larceny --r6rs --program wav.sps -- -extract time1 time2 srcfile dstfile")
;  (display msg0 (current-error-port))
;  (newline (current-error-port))
;  (display msg1 (current-error-port))
;  (newline (current-error-port))
;  (display msg2 (current-error-port))
;  (newline (current-error-port))
;  (display msg3 (current-error-port))
;  (newline (current-error-port))
;  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utilities.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a binary output port and a 4-character string of Ascii
; characters, writes the binary equivalent of the string to the
; port.

(define (putID q id)
  (put-u8 q (char->integer (string-ref id 0)))
  (put-u8 q (char->integer (string-ref id 1)))
  (put-u8 q (char->integer (string-ref id 2)))
  (put-u8 q (char->integer (string-ref id 3))))

; Given a binary output port and a nonnegative 16-bit value,
; writes a little-endian representation of the value to the
; port.

(define (put-u16 q n)
  (let* ((n0 (mod n 256))
         (n1 (mod (div n 256) 256)))
    (put-u8 q n0)
    (put-u8 q n1)))

; Given a binary output port and a nonnegative 32-bit value,
; writes a little-endian representation of the value to the
; port.

(define (put-u32 q n)
  (let* ((n0 (mod n 256))
         (n1 (mod (div n 256) 256))
         (n2 (mod (div n (* 256 256)) 256))
         (n3 (mod (div n (* 256 256 256)) 256)))
    (put-u8 q n0)
    (put-u8 q n1)
    (put-u8 q n2)
    (put-u8 q n3)))



; Given the name of a file, returns its contents as a bytevector.

(define (read-file-as-bytevector file)
  (call-with-port
   (open-file-input-port file)
   (lambda (p)
     (get-bytevector-all p))))

; Given the name of a file to be written, and a vector whose
; contents are exact integers in the range [0,255], writes
; the file with those contents.

(define (write-file-from-bytevector file bv)
  (call-with-port
   (open-file-output-port file)
   (lambda (q)
     (put-bytevector q bv))))

; Displays the contents of a bytevector.

(define (display-bytes bvec . rest)
  (define (display-bytes bvec out)
    (let ((k 16)                    ; bytes to display per line
          (width:offset 6)          ; width of displayed offset
          (n (bytevector-length bvec)))
      (define (display-line i)
        (display-offset i)
        (display "  " out)
        (display-as-characters i)
        (display "  " out)
        (display-as-hexadecimal i)
        (newline out))
      (define (display-offset i)
        (let ((s (number->string i 16)))
          (if (< (string-length s) width:offset)
              (display (make-string (- width:offset (string-length s))
                                    #\space)
                       out)
              '())
          (display s out)))
      (define (display-as-characters i)
        (do ((j 0 (+ j 1)))
            ((= j k))
          (if (< (+ i j) n)
              (let* ((code (bytevector-u8-ref bvec (+ i j)))
                     (c (if (<= 32 code 126) (integer->char code) #\.)))
                (write-char c out))
              (write-char #\space out))))
      (define (display-as-hexadecimal i)
        (do ((j 0 (+ j 1)))
            ((= j k))
          (if (< (+ i j) n)
              (display-byte-as-hexadecimal (bytevector-u8-ref bvec (+ i j)))
              '())))
      (define (display-byte-as-hexadecimal byte)
        (let ((s (number->string (+ (mod byte 256) 256) 16)))
          (display (substring s 1 3) out)))
      (do ((i 0 (+ i k)))
          ((>= i n))
        (display-line i))))
  (cond ((null? rest)
         (display-bytes bvec (current-output-port)))
        ((string? (car rest))
         (call-with-output-file
          (car rest)
          (lambda (out) (display-bytes bvec out))))
        ((output-port? (car rest))
         (display-bytes bvec (car rest)))
        (else
         (error "Bad second argument to display-bytes"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  Parsing a Standard WAVE File
;
;  The "canonical" WAVE file format described at
;  https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
;
;  The file begins with 44 bytes of header followed by the raw
;  sound data.  The first 4 bytes of the file are "RIFF" or "RIFX"
;  in Ascii.  With "RIFF", the integer fields and raw sound data
;  are little-endian.  With "RIFX", they're big-endian.
;
;  byte offset  field size
;       0             4      ChunkID: "RIFF" or "RIFX" in Ascii
;       4             4      ChunkSize: (size of file in bytes) - 8
;       8             4      Format: "WAVE" in Ascii
;
;      12             4      Subchunk1ID: "fmt " in Ascii
;      16             4      Subchunk1Size: normally 16
;      20             2      AudioFormat: 1 means PCM
;      22             2      NumChannels: 1 means mono, 2 means stereo
;      24             4      SampleRate: 44100 for CD-quality
;      28             4      ByteRate: SampleRate*NumChannels*BitsPerSample/8
;      32             2      BlockAlign: NumChannels*BitsPerSample/8
;      34             2      BitsPerSample: 16 for CD-quality
;
;      36             4      Subchunk2ID: "data" in Ascii
;      40             4      Subchunk2Size:
;                                NumSamples*NumChannels*BitsPerSample/8
;                                (which should be the number of bytes
;                                in the rest of the file)
;      44                    encoded data starts here
;                            (with stereo, each sample consists of the
;                            left channel followed by the right channel)
;
;  8-bit samples are unsigned bytes.
;  16-bit samples are signed two's complement.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Displays the header of a WAVE file.

(define (summarize-wave-file filename1)
  (call-with-values
   (lambda () (parse-wave-file-header filename1))
   (lambda (endianness audioFormat numChannels sampleRate byteRate
            blockAlign bits/sample data-start data-end)
     (display filename1)
     (newline)
;    (display "ChunkID:       ") (display ChunkID      ) (newline)
;    (display "ChunkSize:     ") (display ChunkSize    ) (newline)
;    (display "Format:        ") (display Format       ) (newline)
;    (display "Subchunk1ID:   ") (display Subchunk1ID  ) (newline)
;    (display "Subchunk1Size: ") (display Subchunk1Size) (newline)
     (display "AudioFormat:   ") (display audioFormat  ) (newline)
     (display "NumChannels:   ") (display numChannels  ) (newline)
     (display "SampleRate:    ") (display sampleRate   ) (newline)
     (display "ByteRate:      ") (display byteRate     ) (newline)
;    (display "BlockAlign:    ") (display blockAlign   ) (newline)
     (display "BitsPerSample: ") (display bits/sample  ) (newline)
;    (display "Subchunk2ID:   ") (display Subchunk2ID  ) (newline)
;    (display "Subchunk2Size: ") (display Subchunk2Size) (newline)
     (display "Duration:      ")
     (display (inexact (/ (- data-end data-start) blockAlign sampleRate)))
     (newline)
     )))

; Copies a WAVE file from filename1 to filename2,
; omitting all but the essential chunks.

(define (scrub-wave-file filename1 filename2)
  (call-with-port
   (open-file-input-port filename1)
   (lambda (p)
     (if (file-exists? filename2)
         (delete-file filename2)
         '())
     (call-with-port
      (open-file-output-port filename2)
      (lambda (q)
        (scrub-wave-port p q))))))

; Copies the contents of a WAVE file from binary input port p
; to binary output port q, omitting all but the essential chunks.

(define (scrub-wave-port p q)

  (define (scrub-error msg . rest)
    (apply error 'scrub-wave-port msg rest))

  (define (readID)
    (let* ((c0 (get-u8 p))
           (c1 (get-u8 p))
           (c2 (get-u8 p))
           (c3 (get-u8 p)))
      (if (and (number? c0)
               (number? c1)
               (number? c2)
               (number? c3))
          (apply string
                 (map integer->char (list c0 c1 c2 c3)))
          "")))

  (define (putID id)
    (put-u8 q (char->integer (string-ref id 0)))
    (put-u8 q (char->integer (string-ref id 1)))
    (put-u8 q (char->integer (string-ref id 2)))
    (put-u8 q (char->integer (string-ref id 3))))

  (let* ((chunkID (readID))
         (endianness (cond ((string=? chunkID "RIFF") 'little)
                           ((string=? chunkID "RIFX") 'big)
                           (else (scrub-error "bad ChunkID")))))

    (define (get-u16 p)
      (let* ((n0 (get-u8 p))
             (n1 (get-u8 p)))
        (if (and (number? n0)
                 (number? n1))
            (if (eq? endianness 'little)
                (+ n0 (* 256 n1))
                (+ n1 (* 256 n0)))
            0)))

    (define (get-u32 p)
      (let* ((n0 (get-u8 p))
             (n1 (get-u8 p))
             (n2 (get-u8 p))
             (n3 (get-u8 p)))
        (if (and (number? n0)
                 (number? n1)
                 (number? n2)
                 (number? n3))
            (if (eq? endianness 'little)
                (+ n0 (* 256 (+ n1 (* 256 (+ n2 (* 256 n3))))))
                (+ n3 (* 256 (+ n2 (* 256 (+ n1 (* 256 n0)))))))
            0)))

    (define (put-u32 q n)
      (let* ((n0 (mod n 256))
             (n1 (mod (div n 256) 256))
             (n2 (mod (div n (* 256 256)) 256))
             (n3 (mod (div n (* 256 256 256)) 256)))
         (if (eq? endianness 'little)
             (begin (put-u8 q n0)
                    (put-u8 q n1)
                    (put-u8 q n2)
                    (put-u8 q n3))
             (begin (put-u8 q n3)
                    (put-u8 q n2)
                    (put-u8 q n1)
                    (put-u8 q n0)))))

    ; Copies the rest of a chunk (after the 8 bytes of ID and Size)
    ; from p to q.

    (define (copy-chunk n)
      (if (fx>? n 0)
          (let ((x (get-u8 p)))
            (if (fixnum? x)
                (put-u8 q x)
                (scrub-error "bad chunk or i/o error"))
            (copy-chunk (fx- n 1)))
          '()))

    ; Skips the rest of a chunk (after the 8 bytes of ID and Size).

    (define (skip-chunk n)
      (if (fx>? n 0)
          (begin (get-u8 p)
                 (skip-chunk (fx- n 1)))
          '()))

    ; Copies the "fmt " and "data" chunks from p to q
    ; while calculating the ChunkSize for the new file.

    (define (copy-desired-chunks ChunkSize fmtCopied? dataCopied?)
      (if (port-eof? p)
          (begin (set-port-position! p 4)
                 (let ((originalChunkSize (get-u32 p)))
                   (if (not (= originalChunkSize ChunkSize))
                       (begin (set-port-position! q 4)
                              (put-u32 q ChunkSize))
                       '())))
          (let* ((subchunkID (readID))
                 (subchunkSize (get-u32 p)))
            (cond ((and (string=? subchunkID "fmt ")
                        (not fmtCopied?))
                   (putID subchunkID)
                   (put-u32 q subchunkSize)
                   (copy-chunk subchunkSize)
                   (copy-desired-chunks (+ ChunkSize 8 subchunkSize)
                                        #t dataCopied?))
                  ((and (string=? subchunkID "data")
                        (not dataCopied?))
                   (putID subchunkID)
                   (put-u32 q subchunkSize)
                   (copy-chunk subchunkSize)
                   (copy-desired-chunks (+ ChunkSize 8 subchunkSize)
                                        fmtCopied? #t))
                  (else
                   (skip-chunk subchunkSize)
                   (copy-desired-chunks ChunkSize fmtCopied? dataCopied?))))))

    (let* ((chunkSize (get-u32 p))
           (format0 (readID)))
      (cond ((string=? format0 "WAVE")
             (putID chunkID)
             (put-u32 q chunkSize)
             (putID format0)
             (copy-desired-chunks 4 #f #f))
            (else
             (scrub-error "not a WAVE file"))))))

; Extracts the specified portion of a WAVE file from filename1
; with output to filename2.
; The source filename1 must already be in the canonical form
; produced by the --copy operation.
; Always produces a RIFF (little-endian) output file.

(define (extract-from-wave-file time1 time2 filename1 filename2)
  (call-with-values
   (lambda () (parse-wave-file-header filename1))
   (lambda (endianness audioFormat numChannels sampleRate byteRate
            blockAlign bits/sample data-start data-end)
     (let* ((t1 (exact (string->number time1)))  ; FIXME
            (t2 (exact (string->number time2)))  ; FIXME
            (i1 (exact (round (* t1 sampleRate))))
            (i2 (exact (round (* t2 sampleRate))))
            (j1 (* i1 blockAlign))
            (j2 (* i2 blockAlign))
            (k1 (+ j1 data-start))
            (k2 (+ j2 data-start)))
       (if (<= j1 j2 (- data-end data-start))
           (begin
            (if (file-exists? filename2)
                (delete-file filename2)
                '())
            (call-with-port
             (open-file-output-port filename2)
             (lambda (q)
               (putID q "RIFF")
               (put-u32 q (+ -8 44 (- j2 j1)))
               (putID q "WAVE")
               (putID q "fmt ")
               (put-u32 q 16)
               (put-u16 q 1)           ; PCM
               (put-u16 q numChannels)
               (put-u32 q sampleRate)
               (put-u32 q byteRate)
               (put-u16 q blockAlign)
               (put-u16 q bits/sample)
               (putID q "data")
               (put-u32 q (- j2 j1))
               (call-with-port
                (open-file-input-port filename1)
                (lambda (p)
                  (do ((j 0 (fx+ j 1)))
                      ((fx=? j k1))
                    (get-u8 p))
                  (do ((j k1 (fx+ j 1)))
                      ((fx=? j k2))
                    (put-u8 q (get-u8 p))))))))
           (error 'extract-from-wave-file
                  "bad times" time1 time2))))))

; Given the name of a WAVE file, in the minimal canonical format
; created by the --copy operation, returns 9 values:
;
;     endianness (little or big)
;     audioFormat (PCM = 1)
;     numChannels
;     sampleRate
;     byteRate
;     blockAlign
;     bitsPerSample
;     starting offset of data
;     ending offset of data (the length of the file)

(define (parse-wave-file-header filename)
  (call-with-port
   (open-file-input-port filename)
   (lambda (p)
     (parse-wave-header (get-bytevector-n p 44)))))   ; FIXME

(define (parse-wave-header bvec)
  (if (not (>= (bytevector-length bvec) 44))
      (error 'parse-wave-header "WAVE file too short" bvec)
      '())
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(case operation
; ((summarize)
;  (apply summarize-wave-file (cdr cmds)))
; ((copy)
;  (apply scrub-wave-file (cdr cmds)))
; ((extract)
;  (apply extract-from-wave-file (cdr cmds)))
; (else
;  (write operation)
;  (newline)
;  (write cmds)
;  (newline)
;  (print-usage)))

) ; End of (local wav) library
