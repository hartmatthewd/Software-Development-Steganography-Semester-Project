;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; fileio.rkt
;
; Contains various methods for accessing the file system
;
; Clients should use the methods contained herein to
; read/write files rather than accessing the file system 
; directly
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Constructors/Finalizers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Opens a binary input port of the given file
; inputs
;     file (string?) - the path to the file
; ouputs
;     input-port?

(define (open-file-input-port file)
    (open-input-file file #:mode 'binary))

;;;;;;;;;;;;;;;;;;
; Opens a binary output port to the given file
; inputs
;     file (string?) - the path to the file
; outputs
;     output-port?

(define (open-file-output-port file)
    (open-output-file file #:mode 'binary #:exists 'replace))

;;;;;;;;;;;;;;;;;;
; Flush and close the given input port
; inputs
;     input (input-port?) - the input port to finalize
; ouputs
;     void

(define (finalize-input-port input)
    (close-input-port input))

;;;;;;;;;;;;;;;;;;
; Flush and close the given output port
; inputs
;     output (output-port?) - the output port to finalize
; ouputs
;     void

(define (finalize-output-port output)
    (close-output-port output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     File I/O
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Read the given amount of bytes from the given input port
; inputs
;     amt (real?) - the amount of bytes to read
;     in-port (input-port?) - the port to read from
; ouputs
;     bytes?

(define (read-bytes-from-file amt in-port)
    (read-bytes amt in-port))

;;;;;;;;;;;;;;;;;;
; Writes the given bytes to the output port
; inputs
;     bytes (bytes?) - the bytes to write
;     out-port (output-port?) - the output port to write to
; ouputs
;     void

(define (write-bytes-to-file bytes out-port)
    (write-bytes bytes out-port))

;;;;;;;;;;;;;;;;;;
; Read in the remaining bytes from the given input port and write them all to the given output port
;     closes both ports before returning
; inputs
;     in (input-port?) - the input port
;     out (output-port?) - the output port
; ouputs
;     void

(define (pipe-remaining-bytes in out)
  (do [(b (read-byte in) (read-byte in))]
      [(eof-object? b) (begin (finalize-input-port in)
                              (finalize-output-port out))]
      (write-byte b out)))

;;;;;;;;;;;;;;;;;;
; Writes the given bytes to the given file, replacing the existing file if one already exists
; inputs
;     bytes (bytes?) - the bytestring to write
;     file (string?) - the path to a file
; ouputs
;     void

(define (bytes->file bytes file)
    (display-to-file bytes file #:mode 'binary #:exists 'replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         
;;     MP3 and WAV
;;                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
; Given a path to a file, determine if the file represents a .wav file
; inputs
;     file (string?) - the path to a file to test
; ouputs
;     boolean?

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
; Given a path to a file, determine if the file represents a .mp3 file.
;     This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
; inputs
;     file (string?) - the path to a file to test
; ouputs
;     boolean?

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
; Given an input wav file and an output mp3 path, convert the wav to mp3 via the LAME encoder and output it at the given mp3 output path
; inputs
;     wav (string?) - the wav to convert to mp3
;     mp3 (string?) - the path where to place the converted wav
; ouputs
;     void

(define (wav->mp3 wav mp3)
   (let [(cmd (string-append lame-path " " wav " " mp3))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))

;;;;;;;;;;;;;;;;;;
; Given an input mp3 file and an output wav path, convert the mp3 to wav via the LAME encoder and output it at the given wav output path
; inputs
;     mp3 (string?) - the mp3 to convert to wav
;     wav (string?) - the path where to place the converted mp3
; ouputs
;     void

(define (mp3->wav mp3 wav)
   (let [(cmd (string-append lame-path " --decode " mp3 " " wav))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))
