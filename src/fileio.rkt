;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     File I/O
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Opens a binary input port of the given file

(define (open-file-input-port file)
    (open-input-file file #:mode 'binary))

;;;;;;;;;;;;;;;;;;
;;; Opens a binary output port to the given file

(define (open-file-output-port file)
    (open-output-file file #:mode 'binary #:exists 'replace))

;;;;;;;;;;;;;;;;;;
;;; Read the given amount of bytes from the given input port
;;; amt - the amount of bytes to read
;;; in-port - the port to read from

(define (read-bytes-from-file amt in-port)
    (read-bytes amt in-port))

;;;;;;;;;;;;;;;;;;
;;; Writes the given bytes to the output port
;;; bytes - the bytes to write
;;; out-port - the output port to write to

(define (write-bytes-to-file bytes out-port)
    (write-bytes bytes out-port))

;;;;;;;;;;;;;;;;;;
;;; Read in the remaining bytes from the given input port and write them all to the given output port
;;; in - the input port
;;; out - the output port

(define (pipe-remaining-bytes in out)
  (do [(b (read-byte in) (read-byte in))]
      [(eof-object? b) (begin (close-input-port in)
                              (close-output-port out))]
      (write-byte b out)))

;;;;;;;;;;;;;;;;;;
;;; Writes the given bytes to the given file, replacing the existing file if one already exists
;;; bytes - the bytestring to write
;;; file - the path to a file

(define (bytes->file bytes file)
    (display-to-file bytes file #:mode 'binary #:exists 'replace))

;;;;;;;;;;;;;;;;;;
;;; Flush and close the given input port
;;; input - the input port to finalize

(define (finalize-input-port input)
    (close-input-port input))

;;;;;;;;;;;;;;;;;;
;;; lush and close the given output port
;;; output - the output port to finalize

(define (finalize-output-port output)
    (close-output-port output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     MP3 and WAV
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Given a file, determine if the file represents a .wav file
;;; file - the path to a file to test

(define (is-wav? file)
    (let* [(port (open-input-file file))
           (bs (read-bytes 12 port))
           (check (lambda (index value) (= (bytes-ref bs index) (char->integer value))))]
        (close-input-port port)
        (and (check 0 #\R)
            (check 1 #\I)
            (check 2 #\F)
            (or (check 3 #\F) (check 3 #\X))
            (check 8 #\W)
            (check 9 #\A)
            (check 10 #\V)
            (check 11 #\E))))

;;;;;;;;;;;;;;;;;;
;;;Given a file, determine if the file represents a .mp3 file
;;;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
;;; file - the path to a file to test

(define (is-mp3? file)
    (let* [(port (open-input-file file))
           (bs (read-bytes 3 port))
           (firsttwo (bitwise-and (+ (* 256 (bytes-ref bs 0)) (bytes-ref bs 1)) 65534)) ;65534 = 0xFFFE
           (check (lambda (index value) (= (bytes-ref bs index) (char->integer value))))]
          (close-input-port port)
          (or (and (check 0 #\I)
                   (check 1 #\D)
                   (check 2 #\3))
              (or (= firsttwo 65530)     ;0xFFFA MP3 v1.0
                  (= firsttwo 65522)     ;0xFFF2 MP3 v2.0
                  (= firsttwo 65506))))) ;0xFFE2 MP3 v2.5


;;;;;;;;;;;;;;;;;;
;;; Given an input wav file and an output mp3 path, convert the wav to mp3 via the LAME encoder and output it at the given mp3 output path
;;; wav - the wav to convert to mp3
;;; mp3 - the path where to place the converted wav

(define (wav->mp3 wav mp3)
   (let [(cmd (string-append lame-path " " wav " " mp3))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))

;;;;;;;;;;;;;;;;;;
;;; Given an input mp3 file and an output wav path, convert the mp3 to wav via the LAME encoder and output it at the given wav output path
;;; mp3 - the mp3 to convert to wav
;;; wav - the path where to place the converted mp3

(define (mp3->wav mp3 wav)
   (let [(cmd (string-append lame-path " --decode " mp3 " " wav))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))
