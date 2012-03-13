;;;
;;; Functions handling reading the writing of files. Also handles transformation from wav to mp3 and back
;;;

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
   (if (file-exists? file)
       (delete-file file)
       '())
   (display-to-file bytestring file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                                         
;;;;;;;;     MP3 and WAV
;;;;;;;;					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Given a byte string bs, determine if bs represents a .wav file
(define (is-wav? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
        (and (check 0 #\R)
            (check 1 #\I)
            (check 2 #\F)
            (or (check 3 #\F) (check 3 #\X))
            (check 8 #\W)
            (check 9 #\A)
            (check 10 #\V)
            (check 11 #\E))))

;Given a byte string bs, determine if bs represents a .mp3 file
;NOTE: This is a pretty simplistic checking method. It only works for MP3s with ID3 metadata tags.
(define (is-mp3? bs)
    (let ((check (lambda (index value) (= (bytes-ref bs index) (char->integer value)))))
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
