; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

; the number of samples to use for each fft (must be a power of 2)
(define samples-per-fft 512)

; Different temp files to use during encoding/decoding/testing
(define tmpsrc (path->string (make-temporary-file "~a")))
(define tmpdest (path->string (make-temporary-file "~a")))
(define tmptemp (path->string (make-temporary-file "~a.mp3"))) ; must end in .mp3

; Returns the relative path between the current file and the given file
(define (get-relative-path file)
    (string-append (path->string (current-load-relative-directory)) file))

; Samples files to use with testing
(define testwav (get-relative-path "../tst/testwav.wav"))
(define testwav-2 (get-relative-path "../tst/excerpt0.wav"))
(define testmp3 (get-relative-path "../tst/testmp3.mp3"))
(define testmp3-2 (get-relative-path "../tst/brobob.mp3"))
(define testmp3-3 (get-relative-path "../tst/classical.mp3"))
(define testpayload (get-relative-path "../tst/testpayload.txt"))

; The percentage of bits that are allowed to be decoded incorrectly and still pass testing
(define biterrorlimit 0.015)

; An exception thrown when wavfile tries to read more bytes than are left in the file
(define max-pages-exceeded 'max-pages-exceeded)

; pi (3.1415.....)
(define pi (acos -1.0))
(define pi/2 (/ pi 2))
(define pi/4 (/ pi 4))
(define 2pi (* 2 pi))
(define i2pi (* 2.0 pi +1.0i))
(define -pi/4 (- 0 pi/4))
