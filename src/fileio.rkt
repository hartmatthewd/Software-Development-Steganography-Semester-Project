;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     File I/O
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
(define (open-file-input-port file)
    (open-input-file file #:mode 'binary))

;;;;;;;;;;;;;;;;;;
(define (open-file-output-port file)
    (open-output-file file #:mode 'binary #:exists 'replace))

;;;;;;;;;;;;;;;;;;
(define (read-bytes-from-file amt in-port)
    (read-bytes amt in-port))

;;;;;;;;;;;;;;;;;;
(define (write-bytes-to-file bytes out-port)
    (write-bytes bytes out-port))

;;;;;;;;;;;;;;;;;;
(define (pipe-remaining-bytes in out)
  (do [(b (read-byte in) (read-byte in))]
      [(eof-object? b) (begin (close-input-port in)
                              (close-output-port out))]
      (write-byte b out)))

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

;;;;;;;;;;;;;;;;;;
(define (finalize-input-port input)
    (close-input-port input))

;;;;;;;;;;;;;;;;;;
(define (finalize-output-port output)
    (close-output-port output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     MP3 and WAV
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;Given a file, determine if the file represents a .wav file
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
;Given a file, determine if the file represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
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

(define (wav->mp3 wav mp3)
   (let [(cmd (string-append lame-path " " wav " " mp3))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))

;;;;;;;;;;;;;;;;;;
;;; Given an input mp3 file and an output wav path, convert the mp3 to wav via the LAME encoder and output it at the given wav output path

(define (mp3->wav mp3 wav)
   (let [(cmd (string-append lame-path " --decode " mp3 " " wav))]
        (when (not (system cmd))
              (error 'ERROR (string-append "Lame encoder returned an error on command: " cmd)))))
