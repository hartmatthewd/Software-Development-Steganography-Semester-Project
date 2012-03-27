;;; The path where to find the lame encoder
(define lame-path "/course/cs4500wc/bin/lame --quiet -q 9 ")

;;; the number of samples to use for each fft
(define samples-per-fft 512)

(define frequency-components-to-encode (vector 2 3 4))

(define tmpsrc "/tmp/stegosrc")
(define tmpdest "/tmp/stegodest")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;;;;;;;;  Don't Change anything below this line
;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; pi (3.1415.....)
(define pi (acos -1.0))
(define pi/2 (/ pi 2))
(define 3pi/2 (/ (* 3 pi) 2))
(define 2pi (* 2 pi))
(define i2pi (* 2.0 pi +1.0i))

